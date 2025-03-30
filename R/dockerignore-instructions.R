#' Add patterns to a dockerignore object
#'
#' @param dockerignore A dockerignore object
#' @param pattern Character vector of patterns to add
#' @return Updated dockerignore object
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

#' Remove patterns from a dockerignore object
#'
#' @param dockerignore A dockerignore object
#' @param pattern Character vector of patterns to remove
#' @return Updated dockerignore object
#' @export
di_remove <- function(dockerignore, pattern) {
  check_dockerignore(dockerignore)
  
  # Remove patterns if present
  dockerignore$patterns <- dockerignore$patterns[!dockerignore$patterns %in% pattern]
  
  dockerignore
}

#' Replace patterns in a dockerignore object
#'
#' @param dockerignore A dockerignore object
#' @param old_pattern Pattern(s) to replace
#' @param new_pattern New pattern(s)
#' @return Updated dockerignore object
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
