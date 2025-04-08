#' Create a new `dockerignore` object
#'
#' Creates an empty `dockerignore`` object that can be populated with patterns
#' to ignore during Docker builds.
#'
#' @return
#' A `dockerignore` object with a `patterns` character vector
#'
#' @examples
#' # Create a new dockerignore object
#' di <- dockerignore()
#' 
#' # Add patterns
#' di <- di_add(di, c(".git/", "*.log"))
#'
#' @seealso
#' [di_add()] for adding patterns,
#' [write_dockerignore()] for writing to a .dockerignore file, &
#' [dk_template_ignore_common()] for adding common patterns
#'
#' @family dockerignore core functions
#' @export
dockerignore <- function() {
  structure(
    list(
      patterns = character()
    ),
    class = "dockerignore"
  )
}

#' Combine multiple `dockerignore` objects
#'
#' Merges patterns from multiple `dockerignore` objects into a single one.
#' This is useful for combining different template patterns.
#'
#' @param ... `dockerignore` objects to combine
#'
#' @return
#' A new `dockerignore` object with combined patterns from all inputs
#'
#' @examples
#' # Create dockerignore objects with different patterns
#' di_git <- dk_template_ignore_git()
#' di_r <- dk_template_ignore_r()
#' 
#' # Combine them
#' di_combined <- c(di_git, di_r)
#'
#' @seealso
#' [dockerignore()] for creating a new `dockerignore` object &
#' [dk_template_ignore_common()] for adding common patterns
#'
#' @family dockerignore core functions
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
#' Checks whether the provided object is a valid `dockerignore` class object.
#'
#' @param x Object to test
#'
#' @return
#' `TRUE` if `x` is a dockerignore object, `FALSE` otherwise
#'
#' @examples
#' di <- dockerignore()
#' is_dockerignore(di)  # TRUE
#' is_dockerignore(list())  # FALSE
#'
#' @seealso
#' [dockerignore()] for creating a dockerignore object &
#' [check_dockerignore()] for ensuring an object is a dockerignore (with error)
#'
#' @family dockerignore core functions
#' @export
is_dockerignore <- function(x) {
  inherits(x, "dockerignore")
}

#' Ensure an object is a `dockerignore`
#'
#' Verifies that the provided object is a valid `dockerignore` class object,
#' throwing an error if not. Useful for validation inside functions that
#' expect `dockerignore` objects.
#'
#' @param dockerignore Object to check
#'
#' @return
#' Invisibly returns `TRUE` if valid, otherwise throws an error
#'
#' @examples
#' di <- dockerignore()
#' check_dockerignore(di)  # Valid, returns TRUE invisibly
#' 
#' \dontrun{
#' # This would throw an error
#' check_dockerignore(list())
#' }
#'
#' @seealso
#' [is_dockerignore()] for checking if an object is a dockerignore &
#' [dockerignore()] for creating a dockerignore object
#'
#' @family dockerignore core functions
#' @export
check_dockerignore <- function(dockerignore) {
  if (!is_dockerignore(dockerignore)) {
    cli::cli_abort("Expected a dockerignore object, got {class(dockerignore)[1]}")
  }
  invisible(TRUE)
}

#' Print method for dockerignore objects
#'
#' Displays the contents of a dockerignore object in a readable format,
#' showing each pattern on a new line as it would appear in an
#' actual .dockerignore file.
#'
#' @param x A `dockerignore` object
#' @param ... Additional arguments (not used)
#'
#' @return
#' Invisibly returns the dockerignore object
#'
#' @examples
#' di <- dockerignore() |>
#'   di_add(c(".git/", "*.log", "node_modules/"))
#' print(di)
#'
#' @seealso
#' [dockerignore()] for creating a dockerignore object &
#' [write_dockerignore()] for writing to a .dockerignore file
#'
#' @family dockerignore core functions
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