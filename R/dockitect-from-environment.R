#' Create a `dockerfile` from the current R session
#'
#' Generates a `dockerfile` based on the current R session's loaded packages
#' and environment, making it easy to containerize your current workflow.
#'
#' @param base_image       Base image to use
#'                         (default: `"rocker/r-ver"` with current R version)
#' @param include_packages Include loaded packages (default: `TRUE`)
#' @param include_sysreqs  Include system requirements for packages (default: `TRUE`)
#' @param package_manager  Package manager to use (default: `"auto"`)
#'
#' @return
#' A `dockerfile` object based on the current R session
#'
#' @examples
#' \dontrun{
#' # Create a dockerfile from the current session
#' df <- dk_from_session()
#' }
#'
#' @details
#' This function:
#'
#' - Uses the current R version by default (or a specified base image)
#' - Detects loaded packages in the current session
#' - Adds necessary system requirements using the pak package
#' - Adds commands to install the detected R packages
#'
#' The resulting `dockerfile` will recreate an environment similar to your
#' current R session, making it easier to containerize your work.
#'
#' @seealso
#' [dk_from_renv()] for creating from renv.lock files,
#' [dk_from_description()] for creating from DESCRIPTION files, &
#' [dk_add_sysreqs()] for adding system requirements
#'
#' @family dockerfile from environment functions
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

#' Create a `dockerfile` from an **renv.lock** file
#'
#' Generates a `dockerfile` based on an **renv.lock** file, ensuring reproducible
#' package dependencies in the container.
#'
#' @param lock_file       Path to **renv.lock** file
#' @param r_version       R version to use (default: from lock file)
#' @param base_image      Base image to use (default: determined from R version)
#' @param include_sysreqs Include system requirements (default: `TRUE`)
#'
#' @return
#' A `dockerfile` object configured based on the renv.lock file
#'
#' @examples
#' \dontrun{
#' # Create a dockerfile from an renv.lock file
#' df <- dk_from_renv("renv.lock")
#'
#' # Specify a different R version
#' df <- dk_from_renv("renv.lock", r_version = "4.4.0")
#' }
#'
#' @details
#' This function:
#'
#' - Extracts the R version from the lock file (if available)
#' - Uses the appropriate rocker/r-ver base image
#' - Adds system requirements for the packages in the lock file
#' - Sets up renv and restores the packages from the lock file
#'
#' The resulting dockerfile will recreate the exact environment specified
#' in your renv.lock file, ensuring reproducible results.
#'
#' @seealso
#' [dk_from_session()] for creating from the current session,
#' [dk_from_description()] for creating from DESCRIPTION files, &
#' [dk_add_sysreqs()] for adding system requirements
#'
#' @family dockerfile from environment functions
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
        "R -e \"install.packages('renv', repos = 'https://cloud.r-project.org/')\"",
        "R -e \"renv::init()\"",
        "R -e \"renv::restore()\""
      ))
  }

  df
}

#' Create a `dockerfile` from a **DESCRIPTION** file
#'
#' Generates a `dockerfile`` based on a package **DESCRIPTION** file, ensuring all
#' package dependencies are installed in the container.
#'
#' @param description_file Path to **DESCRIPTION** file
#' @param r_version        R version to use (default: from DESCRIPTION)
#' @param base_image       Base image to use (default: determined from R version)
#' @param include_sysreqs  Include system requirements (default: TRUE)
#'
#' @return
#' A `dockerfile` object configured based on the DESCRIPTION file
#'
#' @examples
#' \dontrun{
#' # Create a dockerfile from a DESCRIPTION file
#' df <- dk_from_description("DESCRIPTION")
#' }
#'
#' @details
#' This function:
#'
#' - Extracts the R version from the **DESCRIPTION**'s `Depends` field (if available)
#' - Uses the appropriate `rocker/r-ver` base image
#' - Extracts package dependencies from `Depends`, `Imports`, and `Suggests` fields
#' - Adds system requirements for those packages
#' - Sets up the package installation
#'
#' The resulting `dockerfile` will install all the dependencies required by
#' your package and then install the package itself from the source.
#'
#' @seealso
#' [dk_from_session()] for creating from the current session,
#' [dk_from_renv()] for creating from renv.lock files, &
#' [dk_add_sysreqs()] for adding system requirements
#'
#' @family dockerfile from environment functions
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

#' Create a `dockerfile` from an R script
#'
#' Generates a `dockerfile` based on an R script, analyzing its dependencies
#' and creating a container that can run the script.
#'
#' @param script_file     Path to R script
#' @param base_image      Base image to use (default: latest `rocker/r-ver`)
#' @param include_sysreqs Include system requirements (default: `TRUE`)
#'
#' @return
#' A `dockerfile` object configured to run the specified R script
#'
#' @examples
#' \dontrun{
#' # Create a dockerfile for an R script
#' df <- dk_from_script("analysis.R")
#'
#' # Use a specific base image
#' df <- dk_from_script("analysis.R", base_image = "rocker/tidyverse:4.4.0")
#' }
#'
#' @details
#' This function analyzes the R script to identify package dependencies by
#' scanning for `library()` and `require()` calls. It then:
#'
#' - Creates a dockerfile with the specified base image
#' - Adds necessary system requirements for the detected packages
#' - Installs the required R packages
#' - Copies the script to the container
#' - Sets up a command to run the script
#'
#' The `dockerfile` can be customized further after creation if needed.
#'
#' @seealso
#' [dk_from_session()] for creating from the current session,
#' [dk_from_renv()] for creating from renv.lock files, &
#' [dk_from_description()] for creating from DESCRIPTION files
#'
#' @family dockerfile from environment functions
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
