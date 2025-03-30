#' Read a .dockerignore file
#'
#' @param file Path to .dockerignore file
#' @return A dockerignore object
#' @export
read_dockerignore <- function(file = ".dockerignore") {
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {file}")
  }
  
  # Read lines from file
  lines <- readLines(file)
  
  # Filter out empty lines and comments
  lines <- lines[!grepl("^\\s*$", lines) & !grepl("^\\s*#", lines)]
  
  # Create a new dockerignore object
  di <- dockerignore()
  di$patterns <- lines
  
  di
}

#' Write a dockerignore object to a file
#'
#' @param dockerignore A dockerignore object
#' @param file Output file path (default: ".dockerignore")
#' @return Invisible dockerignore object
#' @export
write_dockerignore <- function(dockerignore, file = ".dockerignore") {
  check_dockerignore(dockerignore)
  
  # Write to file
  writeLines(dockerignore$patterns, file)
  
  cli::cli_alert_success(".dockerignore written to {file}")
  invisible(dockerignore)
}
