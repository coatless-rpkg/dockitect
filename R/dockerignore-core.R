#' Create a new dockerignore object
#'
#' @return A new, empty dockerignore object
#' @export
dockerignore <- function() {
  structure(
    list(
      patterns = character()
    ),
    class = "dockerignore"
  )
}

#' Test if an object is a dockerfile
#'
#' @param x Object to test
#' @return TRUE if x is a dockerfile, FALSE otherwise
#' @export
is_dockerfile <- function(x) {
  inherits(x, "dockerfile")
}

#' Test if an object is a dockerignore
#'
#' @param x Object to test
#' @return TRUE if x is a dockerignore, FALSE otherwise
#' @export
is_dockerignore <- function(x) {
  inherits(x, "dockerignore")
}

#' Ensure an object is a dockerignore
#'
#' @param dockerignore Object to check
#' @return Invisible TRUE if valid, error otherwise
#' @export
check_dockerignore <- function(dockerignore) {
  if (!is_dockerignore(dockerignore)) {
    cli::cli_abort("Expected a dockerignore object, got {class(dockerignore)[1]}")
  }
  invisible(TRUE)
}

#' Print method for dockerignore objects
#'
#' @param x A dockerignore object
#' @param ... Additional arguments (not used)
#' @export
print.dockerignore <- function(x, ...) {
  check_dockerignore(x)
  if (length(x$patterns) == 0) {
    cli::cli_text("Empty .dockerignore")
    return(invisible(x))
  }
  
  cat(paste(x$patterns, collapse = "\n"), "\n")
  invisible(x)
}