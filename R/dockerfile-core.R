#' Create a new `dockerfile` object
#'
#' Creates an empty `dockerfile` object that can be populated with Docker instructions.
#'
#' @return
#' A `dockerfile` object with the following structure:
#' * `lines`: Character vector containing Dockerfile instructions
#' * `metadata`: List containing metadata about the Dockerfile:
#'   * `base_image`: Base image name
#'   * `package_manager`: Package manager type (e.g., "apt", "yum")
#'   * `r_version`: R version (if using a rocker image)
#'   * `distribution`: Linux distribution flavor
#'
#' @examples
#' # Create a new dockerfile
#' df <- dockerfile()
#' 
#' # Add instruction for a base image
#' df <- dfi_from(df, "rocker/r-ver:4.4.0")
#' df
#' 
#' # Add an instruction to run a command to update system packages
#' df <- dfi_run(df, "apt update")
#' df
#'
#' @seealso
#' [is_dockerfile()] for checking if an object is a dockerfile,
#' [dfi_from()] for adding a base image, &
#' [write_dockerfile()] for writing a dockerfile to disk
#'
#' @family dockerfile core functions
#' @export
dockerfile <- function() {
  structure(
    list(
      lines = character(),
      metadata = list(
        base_image = NULL,
        package_manager = NULL,
        r_version = NULL,
        os = NULL
      )
    ),
    class = "dockerfile"
  )
}

#' Test if an object is a `dockerfile`
#'
#' Checks whether the provided object is a valid `dockerfile` class object.
#'
#' @param x Object to test
#'
#' @return
#' `TRUE` if `x` is a `dockerfile` object, `FALSE` otherwise
#'
#' @examples
#' df <- dockerfile()
#' is_dockerfile(df)
#' is_dockerfile(list())
#'
#' @seealso
#' [dockerfile()] for creating a `dockerfile` object &
#' [check_dockerfile()] for ensuring an object is a `dockerfile` (with error)
#'
#' @family dockerfile core functions
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

#' Ensure an object is a `dockerfile`
#'
#' Verifies that the provided object is a valid `dockerfile` class object,
#' throwing an error if not. Useful for validation inside functions that
#' expect `dockerfile` objects.
#'
#' @param dockerfile Object to check
#'
#' @return
#' Invisibly returns `TRUE` if valid, otherwise throws an error
#'
#' @examples
#' df <- dockerfile()
#' check_dockerfile(df)
#' \dontrun{
#' # This would throw an error
#' check_dockerfile(list())
#' }
#'
#' @seealso
#' [is_dockerfile()] for checking if an object is a dockerfile &
#' [dockerfile()] for creating a dockerfile object
#'
#' @family dockerfile core functions
#' @export
check_dockerfile <- function(dockerfile) {
  if (!is_dockerfile(dockerfile)) {
    cli::cli_abort("Expected a dockerfile object, got {class(dockerfile)[1]}")
  }
  invisible(TRUE)
}

#' Print method for `dockerfile` objects
#'
#' Displays the contents of a `dockerfile` object in a readable format,
#' showing each instruction on a new line as it would appear in an
#' actual **Dockerfile**.
#'
#' @param x A `dockerfile` object
#' @param ... Additional arguments (not used)
#'
#' @return
#' Invisibly returns the `dockerfile` object
#'
#' @examples
#' # Create a new dockerfile and add a couple of instructions
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:latest") |>
#'   dfi_run("apt-get update")
#'
#' # Print the dockerfile
#' print(df)
#'
#' @seealso
#' [dockerfile()] for creating a `dockerfile` object
#'
#' @family dockerfile core functions
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

#' Add a line to a `dockerfile` and update metadata
#'
#' Adds a line to a `dockerfile` and updates its metadata based on the instruction.
#' This is an internal function used by the more specific `dfi_*` functions.
#'
#' @param dockerfile A `dockerfile` object
#' @param instruction Docker instruction (e.g., `"FROM"`, `"RUN"`)
#' @param args Arguments for the instruction
#'
#' @return
#' Updated `dockerfile` object with the new line and updated metadata
#'
#' @details
#' This internal function handles:
#' 
#' - Adding the instruction with proper formatting
#' - Handling multi-line arguments with appropriate indentation
#' - Updating metadata for special instructions like `FROM`
#' 
#' For `FROM` instructions, it extracts and stores:
#' 
#' - Base image name
#' - Package manager
#' - Operating system
#' - R version (for `rocker/r-ver:version` images)
#'
#' @seealso
#' [dfi_from()] for adding a FROM instruction &
#' [dfi_run()] for adding a RUN instruction
#'
#' @family dockerfile core functions
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
  
  # Update metadata if necessary to extract 
  # base image, package manager, and R version
  if (instruction == "FROM") {
    # Extract just the image part, not the "AS build" part
    base_image <- strsplit(args, " AS ")[[1]][1]
    dockerfile$metadata$base_image <- base_image
    dockerfile$metadata$package_manager <- determine_package_manager(base_image)
    dockerfile$metadata$distribution <- determine_linux_distribution(base_image)
    
    # Try to extract R version from rocker images
    if (grepl("^rocker/r-ver:", base_image)) {
      r_ver <- sub("^rocker/r-ver:", "", base_image)
      dockerfile$metadata$r_version <- r_ver
    }
  }
  
  dockerfile
}
