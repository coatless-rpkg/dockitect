#' Templates registry for storing custom templates
#'
#' @keywords internal
template_registry <- new.env()

#' Register a custom dockerfile template
#'
#' @param name Template name
#' @param template_fn Function that returns a dockerfile
#' @return Invisible TRUE if successful
#' @export
dk_register_template <- function(name, template_fn) {
  if (!is.function(template_fn)) {
    cli::cli_abort("template_fn must be a function that returns a dockerfile")
  }
  
  template_registry[[name]] <- template_fn
  cli::cli_alert_success("Template '{name}' registered successfully")
  invisible(TRUE)
}

#' Create a dockerfile from a custom template
#'
#' @param template_name Name of the template
#' @param ... Arguments to pass to the template function
#' @return A dockerfile object
#' @export
dk_template_custom <- function(template_name, ...) {
  if (!exists(template_name, envir = template_registry)) {
    cli::cli_abort("Template '{template_name}' not found. Use dk_register_template() to register it.")
  }
  
  template_fn <- get(template_name, envir = template_registry)
  template_fn(...)
}

#' Create a base R dockerfile template
#'
#' @param r_version R version to use (default: current version)
#' @param additional_pkgs Additional R packages to install
#' @return A dockerfile object
#' @export
dk_template_base <- function(r_version = NULL, additional_pkgs = NULL) {
  if (is.null(r_version)) {
    r_version <- paste(R.version$major, R.version$minor, sep = ".")
  }
  
  base_image <- paste0("rocker/r-ver:", r_version)
  
  df <- dockerfile() |>
    dfi_from(base_image) |>
    dfi_label(
      maintainer = Sys.getenv("USER", "unknown"),
      description = "Base R image for analysis"
    ) |>
    dfi_workdir("/app")
  
  # Add additional packages if specified
  if (!is.null(additional_pkgs) && length(additional_pkgs) > 0) {
    # Add system requirements
    df <- dk_add_sysreqs(df, additional_pkgs)
    
    # Install R packages
    pkg_list <- paste0(shQuote(additional_pkgs), collapse = ", ")
    df <- dfi_run(df, paste0("R -e \"install.packages(c(", pkg_list, "), repos='https://cloud.r-project.org/')\""))
  }
  
  # Add script and data directories
  df <- df |>
    dfi_run("mkdir -p /app/scripts /app/data /app/output") |>
    dfi_volume("/app/data") |>
    dfi_volume("/app/output") |>
    dfi_cmd("R --no-save")
  
  df
}

#' Create a Shiny app dockerfile template
#'
#' @param r_version R version to use (default: current version)
#' @param port Port to expose (default: 3838)
#' @param app_dir Local directory with Shiny app (default: ".")
#' @param additional_pkgs Additional R packages to install
#' @return A dockerfile object
#' @export
dk_template_shiny <- function(r_version = NULL, port = 3838, app_dir = ".", additional_pkgs = NULL) {
  if (is.null(r_version)) {
    r_version <- paste(R.version$major, R.version$minor, sep = ".")
  }
  
  # Use rocker/shiny as the base image
  base_image <- paste0("rocker/shiny:", r_version)
  
  # Create base dockerfile
  df <- dockerfile() |>
    dfi_from(base_image) |>
    dfi_label(
      maintainer = Sys.getenv("USER", "unknown"),
      description = "Shiny application"
    )
  
  # Required packages for Shiny
  required_pkgs <- c("shiny")
  
  # Combine with additional packages
  if (!is.null(additional_pkgs)) {
    all_pkgs <- c(required_pkgs, additional_pkgs)
  } else {
    all_pkgs <- required_pkgs
  }
  
  # Add system requirements
  df <- dk_add_sysreqs(df, all_pkgs)
  
  # Install R packages
  pkg_list <- paste0(shQuote(all_pkgs), collapse = ", ")
  df <- dfi_run(df, paste0("R -e \"install.packages(c(", pkg_list, "), repos='https://cloud.r-project.org/')\""))
  
  # Copy app files and set up server
  df <- df |>
    dfi_copy(app_dir, "/srv/shiny-server/app") |>
    dfi_workdir("/srv/shiny-server/app") |>
    dfi_expose(port) |>
    dfi_cmd("shiny-server")
  
  df
}

#' Create a Plumber API dockerfile template
#'
#' @param r_version R version to use (default: current version)
#' @param port Port to expose (default: 8000)
#' @param api_file Path to plumber.R file (default: "plumber.R")
#' @param additional_pkgs Additional R packages to install
#' @return A dockerfile object
#' @export
dk_template_plumber <- function(r_version = NULL, port = 8000, api_file = "plumber.R", additional_pkgs = NULL) {
  if (is.null(r_version)) {
    r_version <- paste(R.version$major, R.version$minor, sep = ".")
  }
  
  # Use rocker/r-ver as the base image
  base_image <- paste0("rocker/r-ver:", r_version)
  
  # Create base dockerfile
  df <- dockerfile() |>
    dfi_from(base_image) |>
    dfi_label(
      maintainer = Sys.getenv("USER", "unknown"),
      description = "Plumber API"
    )
  
  # Required packages for Plumber
  required_pkgs <- c("plumber")
  
  # Combine with additional packages
  if (!is.null(additional_pkgs)) {
    all_pkgs <- c(required_pkgs, additional_pkgs)
  } else {
    all_pkgs <- required_pkgs
  }
  
  # Add system requirements
  df <- dk_add_sysreqs(df, all_pkgs)
  
  # Install R packages
  pkg_list <- paste0(shQuote(all_pkgs), collapse = ", ")
  df <- dfi_run(df, paste0("R -e \"install.packages(c(", pkg_list, "), repos='https://cloud.r-project.org/')\""))
  
  # Copy API file and set up server
  df <- df |>
    dfi_workdir("/app") |>
    dfi_copy(api_file, "/app/plumber.R") |>
    dfi_expose(port) |>
    dfi_cmd(c("R", "-e", paste0("pr <- plumber::plumb('/app/plumber.R'); pr$run(host='0.0.0.0', port=", port, ")")))
  
  df
}