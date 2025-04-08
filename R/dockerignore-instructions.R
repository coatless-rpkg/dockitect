#' Add patterns to a dockerignore object
#'
#' Adds one or more patterns to a `dockerignore` object, avoiding duplicates.
#'
#' @param dockerignore A `dockerignore` object
#' @param pattern      Character vector of patterns to add
#'
#' @return
#' An updated `dockerignore` object with the new patterns added
#'
#' @examples
#' di <- dockerignore()
#' 
#' # Add a single pattern
#' di <- di_add(di, ".git/")
#' 
#' # Add multiple patterns
#' di <- di_add(di, c("*.log", "node_modules/", "*.tmp"))
#'
#' @details
#' Patterns follow the same syntax as `.gitignore` files:
#' * Lines starting with `#` are comments
#' * Blank lines are ignored
#' * Trailing slashes `/` specify directories
#' * Patterns with special characters like `*`, `?`, and `[]` use glob syntax
#' * Lines starting with `!` negate a pattern (include a file that would otherwise be ignored)
#'
#' @seealso
#' [di_remove()] for removing patterns &
#' [di_replace()] for replacing patterns
#'
#' @family dockerignore instruction functions
#' @export
di_add <- function(dockerignore, pattern) {
  check_dockerignore(dockerignore)
  
  # Handle vector input
  for (p in pattern) {
    # Add pattern if not already present
    if (!p %in% dockerignore$patterns) {
      dockerignore$patterns <- c(dockerignore$patterns, p)
    }
  }
  
  dockerignore
}

#' Remove patterns from a `dockerignore` object
#'
#' Removes one or more patterns from a `dockerignore` object.
#'
#' @param dockerignore A `dockerignore` object
#' @param pattern Character vector of patterns to remove
#'
#' @return
#' An updated `dockerignore` object with the specified patterns removed
#'
#' @examples
#' # Create a dockerignore object and add some patterns
#' di <- dockerignore() |>
#'   di_add(c(".git/", "*.log", "node_modules/"))
#' 
#' # Remove a pattern
#' di <- di_remove(di, "*.log")
#'
#' @seealso
#' [di_add()] for adding patterns &
#' [di_replace()] for replacing patterns
#'
#' @family dockerignore instruction functions
#' @export
di_remove <- function(dockerignore, pattern) {
  check_dockerignore(dockerignore)
  
  # Remove patterns if present
  dockerignore$patterns <- dockerignore$patterns[!dockerignore$patterns %in% pattern]
  
  dockerignore
}

#' Replace patterns in a dockerignore object
#'
#' Replaces one or more patterns in a dockerignore object with new patterns.
#'
#' @param dockerignore A `dockerignore` object
#' @param old_pattern Pattern(s) to replace
#' @param new_pattern New pattern(s)
#'
#' @return
#' An updated `dockerignore` object with the specified patterns replaced
#'
#' @examples
#' # Create a dockerignore object and add some patterns
#' di <- dockerignore() |>
#'   di_add(c("*.log", "*.tmp", "node_modules/"))
#' 
#' # Replace a single pattern
#' di <- di_replace(di, "*.log", "logs/")
#' 
#' # Replace multiple patterns with a single pattern
#' di <- di_replace(di, c("*.tmp", "node_modules/"), "temp/")
#' 
#' # Replace patterns one-to-one
#' di <- di_replace(di, 
#'                 c("*.log", "*.tmp"), 
#'                 c("logs/*", "temp/*"))
#'
#' @details
#' This function allows you to replace patterns in a dockerignore object.
#' Three modes of operation are supported:
#' 
#' 1. Replace a single pattern with a single pattern
#' 2. Replace multiple patterns with a single pattern
#' 3. Replace multiple patterns with corresponding new patterns (one-to-one)
#'
#' For the third mode, `old_pattern` and `new_pattern` must have the same length.
#'
#' @seealso
#' [di_add()] for adding patterns &
#' [di_remove()] for removing patterns
#'
#' @family dockerignore instruction functions
#' @export
di_replace <- function(dockerignore, old_pattern, new_pattern) {
  check_dockerignore(dockerignore)
  
  # Handle different input cases
  if (length(old_pattern) == 1 && length(new_pattern) == 1) {
    # Simple case: replace single pattern with single pattern
    idx <- which(dockerignore$patterns == old_pattern)
    if (length(idx) > 0) {
      dockerignore$patterns[idx] <- new_pattern
    }
  } else if (length(old_pattern) > 1 && length(new_pattern) == 1) {
    # Replace multiple patterns with a single pattern
    for (op in old_pattern) {
      idx <- which(dockerignore$patterns == op)
      if (length(idx) > 0) {
        dockerignore$patterns[idx] <- new_pattern
      }
    }
  } else if (length(old_pattern) == length(new_pattern)) {
    # Replace each old pattern with corresponding new pattern
    for (i in seq_along(old_pattern)) {
      idx <- which(dockerignore$patterns == old_pattern[i])
      if (length(idx) > 0) {
        dockerignore$patterns[idx] <- new_pattern[i]
      }
    }
  } else {
    cli::cli_abort("old_pattern and new_pattern must have the same length or new_pattern must be length 1")
  }
  
  dockerignore
}
