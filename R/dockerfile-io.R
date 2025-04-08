#' Read a Dockerfile from a file
#'
#' @param file Path to Dockerfile
#' @return A dockerfile object
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

#' Write a dockerfile to a file
#'
#' @param dockerfile A dockerfile object
#' @param file Output file path (default: "Dockerfile")
#' @param multiline Logical indicating if long RUN commands should be split (default: TRUE)
#' @return Invisible dockerfile object
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