#' Read a Dockerfile from a file
#'
#' Parses a **Dockerfile** from disk into a `dockerfile` object that can be
#' manipulated programmatically.
#'
#' @param file Path to **Dockerfile**
#'
#' @return
#' A `dockerfile` object containing the parsed instructions and metadata
#'
#' @examples
#' \dontrun{
#' # Read an existing Dockerfile
#' df <- read_dockerfile("path/to/Dockerfile")
#' 
#' # Modify it
#' df <- dfi_run(df, "apt-get update")
#' df
#' }
#'
#' @details
#' The function handles line continuations and extracts metadata like the
#' base image, package manager, OS, and R version (if applicable).
#' Comments and empty lines are skipped.
#'
#' @seealso
#' [dockerfile()] for creating a new dockerfile object &
#' [write_dockerfile()] for writing a dockerfile to disk
#'
#' @family dockerfile I/O functions
#' @export
read_dockerfile <- function(file) {
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {file}")
  }
  
  # Read lines from file
  lines <- readLines(file)
  
  # Create a new dockerfile object
  df <- dockerfile()
  
  # Process continuation lines
  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]
    
    # Skip empty lines and comments
    if (grepl("^\\s*$", line) || grepl("^\\s*#", line)) {
      i <- i + 1
      next
    }
    
    # Check for continuation character at the end
    while (grepl("\\\\\\s*$", line) && i < length(lines)) {
      # Remove continuation character
      line <- sub("\\\\\\s*$", " ", line)
      # Add next line
      i <- i + 1
      line <- paste0(line, lines[i])
    }
    
    # Add processed line to dockerfile
    df$lines <- c(df$lines, line)
    
    # Update metadata
    if (grepl("^FROM ", line, ignore.case = TRUE)) {
      base_image <- sub("^FROM\\s+([^\\s]+).*$", "\\1", line, ignore.case = TRUE)
      df$metadata$base_image <- base_image
      df$metadata$package_manager <- determine_package_manager(base_image)
      df$metadata$distribution <- determine_linux_distribution(base_image)
      
      # Try to extract R version from rocker images
      if (grepl("^rocker/r-ver:", base_image)) {
        r_ver <- sub("^rocker/r-ver:", "", base_image)
        df$metadata$r_version <- r_ver
      }
    }
    
    i <- i + 1
  }
  
  df
}

#' Write a `dockerfile` to a file
#'
#' Writes a `dockerfile` object to disk as a **Dockerfile**.
#'
#' @param dockerfile A `dockerfile` object
#' @param file Output file path (default: "Dockerfile")
#' @param multiline Logical indicating if long RUN commands should be split (default: TRUE)
#'
#' @return
#' Invisibly returns the `dockerfile` object
#'
#' @examples
#' \dontrun{
#' # Create and write a simple Dockerfile
#' dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_run("apt-get update") |>
#'   write_dockerfile()
#'   
#' # Specify a different file name
#' dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   write_dockerfile("Dockerfile.dev")
#' }
#'
#' @details
#' When `multiline = TRUE` (the default), long `RUN` commands with `&&` 
#' will be formatted with line continuations (`\`) for better readability.
#' This makes the Dockerfile more maintainable without changing its functionality.
#'
#' @seealso
#' [read_dockerfile()] for reading a Dockerfile from disk &
#' [dockerfile()] for creating a new dockerfile object
#'
#' @family dockerfile I/O functions
#' @export
write_dockerfile <- function(dockerfile, file = "Dockerfile", multiline = TRUE) {
  check_dockerfile(dockerfile)
  
  if (multiline) {
    # Process lines for better formatting
    formatted_lines <- character(0)
    
    i <- 1
    while (i <= length(dockerfile$lines)) {
      line <- dockerfile$lines[i]
      
      # Check if it's a RUN command with && that could be split
      if (grepl("^RUN ", line) && nchar(line) > 80 && grepl(" && ", line)) {
        # Extract the command part (remove RUN prefix)
        command <- sub("^RUN\\s+", "", line)
        
        # Split by && and trim whitespace
        parts <- trimws(strsplit(command, " && ")[[1]])
        
        # Filter out empty parts
        parts <- parts[parts != ""]
        
        # Format with correct continuation
        if (length(parts) > 1) {
          formatted_line <- paste0(
            "RUN ", parts[1], " && \\\n",
            paste0("    ", parts[-1], collapse = " && \\\n")
          )
        } else {
          formatted_line <- paste0("RUN ", parts[1])
        }
        
        formatted_lines <- c(formatted_lines, formatted_line)
      } else {
        formatted_lines <- c(formatted_lines, line)
      }
      
      i <- i + 1
    }
    
    # Write to file
    writeLines(formatted_lines, file)
  } else {
    # Write directly without formatting
    writeLines(dockerfile$lines, file)
  }
  
  cli::cli_alert_success("Dockerfile written to {file}")
  invisible(dockerfile)
}