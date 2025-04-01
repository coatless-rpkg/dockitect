#' Create a dockerfile from the current R session
#'
#' @param base_image Base image to use (default: rocker/r-ver with current R version)
#' @param include_packages Include loaded packages (default: TRUE)
#' @param include_sysreqs Include system requirements for packages (default: TRUE)
#' @param package_manager Package manager to use (default: auto-detected)
#' @return A dockerfile object
#' @export
dk_from_session <- function(base_image = NULL, include_packages = TRUE, 
                            include_sysreqs = TRUE, package_manager = "auto") {
  # Get current R version
  r_version <- paste(R.version$major, R.version$minor, sep = ".")
  
  # Set default base image if not provided
  if (is.null(base_image)) {
    base_image <- paste0("rocker/r-ver:", r_version)
  }
  
  # Create base dockerfile
  df <- dockerfile() |>
    dfi_from(base_image) |>
    dfi_label(maintainer = Sys.getenv("USER", "unknown"))
  
  # Set package manager
  if (package_manager == "auto") {
    package_manager <- determine_package_manager(base_image)
  }
  
  # Add loaded packages if requested
  if (include_packages) {
    # Get loaded packages from the current session
    pkgs <- sort(setdiff(
      # Get names of all loaded packages
      loadedNamespaces(),
      # Exclude base R packages and dockitect
      c("base", "compiler", "datasets", "graphics", 
        "grDevices", "grid", "methods", "parallel", 
        "splines", "stats", "stats4", "tcltk", "tools", 
        "utils", "dockitect")
    ))
    
    # Add system requirements if requested
    if (include_sysreqs && length(pkgs) > 0) {
      df <- dk_add_sysreqs(df, pkgs, package_manager)
    }
    
    # Add R package installation command
    if (length(pkgs) > 0) {
      pkg_list <- paste0(shQuote(pkgs), collapse = ", ")
      df <- dfi_run(df, paste0("R -e \"install.packages(c(", pkg_list, "), repos='https://cloud.r-project.org/')\""))
    }
  }
  
  df
}

#' Add system requirements for R packages to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param packages Character vector of package names
#' @param package_manager Package manager to use (default: auto-detected)
#' @return Updated dockerfile object
#' @export
dk_add_sysreqs <- function(dockerfile, packages, package_manager = "auto") {
  check_dockerfile(dockerfile)
  
  if (package_manager == "auto") {
    package_manager <- dockerfile$metadata$package_manager
    if (is.null(package_manager)) {
      package_manager <- "apt"
      cli::cli_warn("Could not determine package manager. Defaulting to apt.")
    }
  }
  
  # Get system requirements using pak
  if (!requireNamespace("pak", quietly = TRUE)) {
    cli::cli_warn("Package 'pak' is required to determine system requirements. Skipping.")
    return(dockerfile)
  }
  
  # Map package manager to appropriate sysreqs platform
  os <- determine_os(dockerfile$metadata$base_image)
  platform <- map_to_sysreqs_platform(package_manager, os)
  
  # Get system requirements data frame
  sysreqs_df <- pak::pkg_sysreqs(packages, sysreqs_platform = platform)
  
  if (is.null(sysreqs_df$packages) || nrow(sysreqs_df$packages) == 0) {
    return(dockerfile)
  }
  
  # Extract system packages from the data frame and remove duplicates
  system_packages <- unique(unlist(sysreqs_df$packages$system_packages))
  
  if (length(system_packages) == 0) {
    return(dockerfile)
  }
  
  # Generate system-specific install commands using utility function
  install_cmd <- generate_pkg_install_cmd(package_manager, system_packages)
  
  # Add the installation command
  dockerfile <- dfi_run(dockerfile, paste(install_cmd, collapse = " && "))
  
  dockerfile
}

#' Create a dockerfile from an renv.lock file
#'
#' @param lock_file Path to renv.lock file
#' @param r_version R version to use (default: from lock file)
#' @param base_image Base image to use (default: determined from R version)
#' @param include_sysreqs Include system requirements (default: TRUE)
#' @return A dockerfile object
#' @export
dk_from_renv <- function(lock_file = "renv.lock", r_version = NULL, 
                         base_image = NULL, include_sysreqs = TRUE) {
  if (!file.exists(lock_file)) {
    cli::cli_abort("Lock file not found: {lock_file}")
  }
  
  # Read lock file
  lock_data <- jsonlite::fromJSON(lock_file)
  
  # Extract R version if not provided
  if (is.null(r_version) && !is.null(lock_data$R$Version)) {
    r_version <- lock_data$R$Version
  }
  
  # Set default base image if not provided
  if (is.null(base_image) && !is.null(r_version)) {
    base_image <- paste0("rocker/r-ver:", r_version)
  } else if (is.null(base_image)) {
    base_image <- "rocker/r-ver:latest"
    cli::cli_warn("R version not specified. Using {base_image}")
  }
  
  # Create base dockerfile
  df <- dockerfile() |>
    dfi_from(base_image) |>
    dfi_label(maintainer = Sys.getenv("USER", "unknown"))
  
  # Add packages from lock file
  if (!is.null(lock_data$Packages) && length(lock_data$Packages) > 0) {
    pkgs <- names(lock_data$Packages)
    
    # Add system requirements if requested
    if (include_sysreqs && length(pkgs) > 0) {
      df <- dk_add_sysreqs(df, pkgs)
    }
    
    # Add renv initialization and restore
    df <- df |>
      dfi_workdir("/app") |>
      dfi_copy(lock_file, "/app/renv.lock") |>
      dfi_run(c(
        "R -e \"install.packages('renv', repos = 'https://cloud.r-project.org/')\" \\",
        "R -e \"renv::init()\" \\",
        "R -e \"renv::restore()\""
      ))
  }
  
  df
}

#' Create a dockerfile from a DESCRIPTION file
#'
#' @param description_file Path to DESCRIPTION file
#' @param r_version R version to use (default: from DESCRIPTION)
#' @param base_image Base image to use (default: determined from R version)
#' @param include_sysreqs Include system requirements (default: TRUE)
#' @return A dockerfile object
#' @export
dk_from_description <- function(description_file = "DESCRIPTION", r_version = NULL,
                                base_image = NULL, include_sysreqs = TRUE) {
  if (!file.exists(description_file)) {
    cli::cli_abort("DESCRIPTION file not found: {description_file}")
  }
  
  # Read DESCRIPTION file
  desc_data <- read.dcf(description_file)
  
  # Extract R version if not provided
  if (is.null(r_version) && "Depends" %in% colnames(desc_data)) {
    r_depends <- desc_data[1, "Depends"]
    r_ver_match <- regexpr("R \\(>= ([0-9\\.]+)\\)", r_depends)
    if (r_ver_match != -1) {
      r_version <- sub("R \\(>= ([0-9\\.]+)\\)", "\\1", regmatches(r_depends, r_ver_match))
    }
  }
  
  # Set default base image if not provided
  if (is.null(base_image) && !is.null(r_version)) {
    base_image <- paste0("rocker/r-ver:", r_version)
  } else if (is.null(base_image)) {
    base_image <- "rocker/r-ver:latest"
    cli::cli_warn("R version not specified. Using {base_image}")
  }
  
  # Create base dockerfile
  df <- dockerfile() |>
    dfi_from(base_image) |>
    dfi_label(maintainer = if ("Maintainer" %in% colnames(desc_data)) desc_data[1, "Maintainer"] else Sys.getenv("USER", "unknown"))
  
  # Extract packages from Depends, Imports, and Suggests
  pkg_fields <- c("Depends", "Imports", "Suggests")
  pkgs <- character(0)
  
  for (field in pkg_fields) {
    if (field %in% colnames(desc_data)) {
      field_content <- desc_data[1, field]
      if (!is.na(field_content)) {
        # Split on commas and extract package names
        field_pkgs <- strsplit(field_content, ",")[[1]]
        field_pkgs <- trimws(field_pkgs)
        # Remove version specifications and R dependency
        field_pkgs <- sub("\\s*\\(.*\\)$", "", field_pkgs)
        field_pkgs <- field_pkgs[!grepl("^R$", field_pkgs)]
        pkgs <- c(pkgs, field_pkgs)
      }
    }
  }
  
  # Add system requirements if requested
  if (include_sysreqs && length(pkgs) > 0) {
    df <- dk_add_sysreqs(df, pkgs)
  }
  
  # Add R package installation command
  if (length(pkgs) > 0) {
    pkg_list <- paste0(shQuote(pkgs), collapse = ", ")
    df <- dfi_run(df, paste0("R -e \"install.packages(c(", pkg_list, "), repos='https://cloud.r-project.org/')\""))
  }
  
  # Add package directory
  pkg_name <- desc_data[1, "Package"]
  df <- df |>
    dfi_workdir("/app") |>
    dfi_copy(".", "/app/") |>
    dfi_run(paste0("R CMD INSTALL --no-multiarch --with-keep.source /app"))
  
  df
}

#' Create a dockerfile from an R script
#'
#' @param script_file Path to R script
#' @param base_image Base image to use (default: latest rocker/r-ver)
#' @param include_sysreqs Include system requirements (default: TRUE)
#' @return A dockerfile object
#' @export
dk_from_script <- function(script_file, base_image = "rocker/r-ver:latest", include_sysreqs = TRUE) {
  if (!file.exists(script_file)) {
    cli::cli_abort("Script file not found: {script_file}")
  }
  
  # Read script file
  script <- readLines(script_file)
  
  # Extract library and require calls to identify packages
  # Consider adding a dependency on `renv` to handle this kind of detection.
  library_pattern <- "library\\(([^\\)]+)\\)"
  require_pattern <- "require\\(([^\\)]+)\\)"
  
  library_matches <- regmatches(script, gregexpr(library_pattern, script))
  require_matches <- regmatches(script, gregexpr(require_pattern, script))
  
  pkgs <- character(0)
  
  # Process library calls
  for (match_list in library_matches) {
    if (length(match_list) > 0) {
      for (match in match_list) {
        pkg_name <- sub(library_pattern, "\\1", match)
        # Remove quotes if present
        pkg_name <- gsub("[\"']", "", pkg_name)
        pkgs <- c(pkgs, pkg_name)
      }
    }
  }
  
  # Process require calls
  for (match_list in require_matches) {
    if (length(match_list) > 0) {
      for (match in match_list) {
        pkg_name <- sub(require_pattern, "\\1", match)
        # Remove quotes if present
        pkg_name <- gsub("[\"']", "", pkg_name)
        pkgs <- c(pkgs, pkg_name)
      }
    }
  }
  
  # Create base dockerfile
  df <- dockerfile() |>
    dfi_from(base_image) |>
    dfi_label(maintainer = Sys.getenv("USER", "unknown"))
  
  # Add system requirements if requested
  if (include_sysreqs && length(pkgs) > 0) {
    df <- dk_add_sysreqs(df, pkgs)
  }
  
  # Add R package installation command
  if (length(pkgs) > 0) {
    pkgs <- unique(pkgs)
    pkg_list <- paste0(shQuote(pkgs), collapse = ", ")
    df <- dfi_run(df, paste0("R -e \"install.packages(c(", pkg_list, "), repos='https://cloud.r-project.org/')\""))
  }
  
  # Add script execution
  script_name <- basename(script_file)
  df <- df |>
    dfi_workdir("/app") |>
    dfi_copy(script_file, paste0("/app/", script_name)) |>
    dfi_cmd(paste0("Rscript /app/", script_name))
  
  df
}