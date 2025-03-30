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

#' Combine multiple dockerignore objects
#'
#' @param ... Dockerignore objects to combine
#' @return A new dockerignore object with combined patterns
#' @export
c.dockerignore <- function(...) {
  args <- list(...)
  
  # Ensure all arguments are dockerignore objects
  is_di <- vapply(args, is_dockerignore, logical(1))
  if (!all(is_di)) {
    non_di <- which(!is_di)
    cli::cli_abort("All arguments must be dockerignore objects (argument {non_di} is not)")
  }
  
  # Create a new dockerignore object
  result <- dockerignore()
  
  # Combine patterns from all objects
  for (di in args) {
    result <- di_add(result, di$patterns)
  }
  
  result
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