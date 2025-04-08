#' Read a **.dockerignore** file
#'
#' Reads a **.dockerignore** file from disk into a `dockerignore` object that
#' can be manipulated programmatically.
#'
#' @param file Path to **.dockerignore** file (default: `".dockerignore"`)
#'
#' @return
#' A `dockerignore` object containing the parsed patterns
#'
#' @examples
#' \dontrun{
#' # Read an existing .dockerignore file
#' di <- read_dockerignore()
#' 
#' # Add more patterns
#' di <- di_add(di, "*.tmp")
#' }
#'
#' @details
#' Empty lines and comments (lines starting with `#`) are filtered out.
#'
#' @seealso
#' [dockerignore()] for creating a new dockerignore object &
#' [write_dockerignore()] for writing a dockerignore to disk
#'
#' @family dockerignore I/O functions
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

#' Write a `dockerignore` object to a file
#'
#' Writes a `dockerignore` object to disk as a formatted **.dockerignore** file.
#'
#' @param dockerignore A `dockerignore` object
#' @param file Output file path (default: `".dockerignore"`)
#'
#' @return
#' Invisibly returns the `dockerignore` object
#'
#' @examples
#' \dontrun{
#' # Create and write a .dockerignore file
#' dockerignore() |>
#'   di_add(c(".git/", "*.log")) |>
#'   write_dockerignore()
#' }
#'
#' @seealso
#' [read_dockerignore()] for reading a .dockerignore file from disk &
#' [dockerignore()] for creating a new dockerignore object
#'
#' @family dockerignore I/O functions
#' @export
write_dockerignore <- function(dockerignore, file = ".dockerignore") {
  check_dockerignore(dockerignore)
  
  # Write to file
  writeLines(dockerignore$patterns, file)
  
  cli::cli_alert_success(".dockerignore written to {file}")
  invisible(dockerignore)
}
