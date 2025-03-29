#' Create a new dockerfile object
#'
#' @return A new, empty dockerfile object
#' @export
dockerfile <- function() {
  structure(
    list(
      lines = character(),
      metadata = list(
        base_image = NULL,
        package_manager = NULL,
        r_version = NULL
      )
    ),
    class = "dockerfile"
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


#' Check if a dockerfile has a specific instruction
#'
#' @param dockerfile A dockerfile object
#' @param instruction Instruction to check for (e.g., "FROM", "RUN")
#' @return TRUE if instruction exists, FALSE otherwise
#' @export
has_instruction <- function(dockerfile, instruction) {
  check_dockerfile(dockerfile)
  instruction <- toupper(instruction)
  any(grepl(paste0("^", instruction, " "), dockerfile$lines))
}

#' Ensure an object is a dockerfile
#'
#' @param dockerfile Object to check
#' @return Invisible TRUE if valid, error otherwise
#' @export
check_dockerfile <- function(dockerfile) {
  if (!is_dockerfile(dockerfile)) {
    cli::cli_abort("Expected a dockerfile object, got {class(dockerfile)[1]}")
  }
  invisible(TRUE)
}

#' Print method for dockerfile objects
#'
#' @param x A dockerfile object
#' @param ... Additional arguments (not used)
#' @export
print.dockerfile <- function(x, ...) {
  check_dockerfile(x)
  if (length(x$lines) == 0) {
    cli::cli_text("Empty Dockerfile")
    return(invisible(x))
  }
  
  cat(paste(x$lines, collapse = "\n"), "\n")
  invisible(x)
}



#' Add a line to a dockerfile and update metadata
#'
#' @param dockerfile A dockerfile object
#' @param instruction Docker instruction (e.g., "FROM", "RUN")
#' @param args Arguments for the instruction
#' @return Updated dockerfile object
#' @keywords internal
add_dockerfile_line <- function(dockerfile, instruction, args) {
  check_dockerfile(dockerfile)
  instruction <- toupper(instruction)
  
  # Handle multi-line arguments
  if (length(args) > 1) {
    # First line with instruction
    line1 <- paste0(instruction, " ", args[1])
    # Other lines indented
    other_lines <- paste0("    ", args[-1])
    lines <- c(line1, other_lines)
    dockerfile$lines <- c(dockerfile$lines, lines)
  } else {
    dockerfile$lines <- c(dockerfile$lines, paste0(instruction, " ", args))
  }
  
  # Update metadata
  if (instruction == "FROM") {
    dockerfile$metadata$base_image <- args[1]
    dockerfile$metadata$package_manager <- get_package_manager(args[1])
    
    # Try to extract R version from rocker images
    if (grepl("^rocker/r-ver:", args[1])) {
      r_ver <- sub("^rocker/r-ver:", "", args[1])
      dockerfile$metadata$r_version <- r_ver
    }
  }
  
  dockerfile
}
