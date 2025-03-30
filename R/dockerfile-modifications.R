#' Add a line to a dockerfile at a specific position
#'
#' @param dockerfile A dockerfile object
#' @param line Line to add
#' @param after Position after which to add the line (default: end)
#' @return Updated dockerfile object
#' @export
#' @keywords internal
dfm_add_line <- function(dockerfile, line, after = NULL) {
  check_dockerfile(dockerfile)
  
  if (is.null(after) || after > length(dockerfile$lines)) {
    dockerfile$lines <- c(dockerfile$lines, line)
  } else {
    before <- dockerfile$lines[1:after]
    after <- if (after < length(dockerfile$lines)) dockerfile$lines[(after+1):length(dockerfile$lines)] else character(0)
    dockerfile$lines <- c(before, line, after)
  }
  
  dockerfile
}

#' Remove a line from a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param line_num Line number to remove
#' @return Updated dockerfile object
#' @export
dfm_remove_line <- function(dockerfile, line_num) {
  check_dockerfile(dockerfile)
  
  if (line_num < 1 || line_num > length(dockerfile$lines)) {
    cli::cli_warn("Line {line_num} is out of range")
    return(dockerfile)
  }
  
  dockerfile$lines <- dockerfile$lines[-line_num]
  dockerfile
}

#' Replace a line in a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param line_num Line number to replace
#' @param new_line New line content
#' @return Updated dockerfile object
#' @export
dfm_replace_line <- function(dockerfile, line_num, new_line) {
  check_dockerfile(dockerfile)
  
  if (line_num < 1 || line_num > length(dockerfile$lines)) {
    cli::cli_warn("Line {line_num} is out of range")
    return(dockerfile)
  }
  
  dockerfile$lines[line_num] <- new_line
  dockerfile
}

#' Move a line in a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param from Source line number
#' @param to Target position
#' @return Updated dockerfile object
#' @export
dfm_move_line <- function(dockerfile, from, to) {
  check_dockerfile(dockerfile)
  
  # Get number of lines
  n_lines <- length(dockerfile$lines)
  
  # Handle out-of-range cases with warnings
  if (from < 1 || from > n_lines) {
    cli::cli_warn("Source line {from} is out of range")
    return(dockerfile)
  }
  
  if (to < 1 || to > n_lines) {
    cli::cli_warn("Target position {to} is out of range")
    return(dockerfile)
  }
  
  # No change needed if source and target are the same
  if (from == to) {
    return(dockerfile)
  }
  
  # Create a copy of lines
  lines <- dockerfile$lines
  
  # Extract the line to move
  line_to_move <- lines[from]
  
  # Remove the line from its original position
  lines <- lines[-from]
  
  # Insert the line at the new position
  lines <- append(lines, values = line_to_move, after = to - 1)

  # Update the dockerfile
  dockerfile$lines <- lines
  
  dockerfile
}

#' Group similar instructions in a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @return Updated dockerfile object with similar instructions grouped
#' @export
dfm_group_similar <- function(dockerfile) {
  check_dockerfile(dockerfile)
  
  if (length(dockerfile$lines) <= 1) {
    return(dockerfile)
  }
  
  # Extract instruction type for each line
  get_instruction <- function(line) {
    if (grepl("^\\s+", line)) {
      # Continuation line
      return(NA_character_)
    }
    parts <- strsplit(line, "\\s+")[[1]]
    return(parts[1])
  }
  
  instructions <- sapply(dockerfile$lines, get_instruction)
  
  # Identify groups to combine
  i <- 1
  result <- dockerfile()
  
  while (i <= length(dockerfile$lines)) {
    current_instr <- instructions[i]
    
    # Skip continuation lines or instructions we don't want to group
    if (is.na(current_instr) || 
        current_instr %in% c("FROM", "WORKDIR", "USER", "ENTRYPOINT", "CMD")) {
      result$lines <- c(result$lines, dockerfile$lines[i])
      i <- i + 1
      next
    }
    
    # Check for consecutive instructions of the same type
    j <- i + 1
    consecutive <- character(0)
    
    while (j <= length(dockerfile$lines) && (instructions[j] == current_instr || is.na(instructions[j]))) {
      if (is.na(instructions[j])) {
        # Add continuation line to previous instruction
        consecutive <- c(consecutive, dockerfile$lines[j])
      } else {
        # New instruction of the same type
        if (current_instr == "RUN") {
          # For RUN, combine with &&
          instr_content <- sub("^RUN\\s+", "", dockerfile$lines[j])
          consecutive <- c(consecutive, paste("&&", instr_content))
        } else if (current_instr %in% c("COPY", "ADD")) {
          # For COPY/ADD, keep separate
          result$lines <- c(result$lines, dockerfile$lines[j])
        } else {
          # For others, just keep separate
          result$lines <- c(result$lines, dockerfile$lines[j])
        }
      }
      j <- j + 1
    }
    
    if (length(consecutive) > 0 && current_instr == "RUN") {
      # Combine RUN instructions
      combined <- paste0(
        dockerfile$lines[i], " \\",
        paste0(sapply(consecutive, function(x) paste0("\n    ", x)), collapse = " \\")
      )
      result$lines <- c(result$lines, combined)
    } else {
      # Just add the current instruction
      result$lines <- c(result$lines, dockerfile$lines[i])
    }
    
    # Move to next group
    i <- j
  }
  
  # Update metadata
  result$metadata <- dockerfile$metadata
  result
}

#' Sort instructions in a dockerfile by type
#'
#' @param dockerfile A dockerfile object
#' @param order Custom order for instructions (optional)
#' @return Updated dockerfile with sorted instructions
#' @export
dfm_sort_by_instruction <- function(dockerfile, order = NULL) {
  check_dockerfile(dockerfile)
  
  if (length(dockerfile$lines) <= 1) {
    return(dockerfile)
  }
  
  # Default instruction order
  if (is.null(order)) {
    order <- c("FROM", "ARG", "LABEL", "ENV", "WORKDIR", 
               "COPY", "ADD", "RUN", "EXPOSE", "VOLUME", 
               "USER", "HEALTHCHECK", "ENTRYPOINT", "CMD")
  }
  
  # Extract instruction type for each line
  get_instruction <- function(line) {
    if (grepl("^\\s+", line)) {
      # Continuation line
      return(NA_character_)
    }
    parts <- strsplit(line, "\\s+")[[1]]
    return(parts[1])
  }
  
  instructions <- sapply(dockerfile$lines, get_instruction)
  
  # Collect continuation lines with their main instruction
  i <- 1
  grouped_lines <- list()
  
  while (i <= length(dockerfile$lines)) {
    current_lines <- dockerfile$lines[i]
    j <- i + 1
    
    # Find continuation lines
    while (j <= length(dockerfile$lines) && is.na(instructions[j])) {
      current_lines <- c(current_lines, dockerfile$lines[j])
      j <- j + 1
    }
    
    grouped_lines[[length(grouped_lines) + 1]] <- list(
      instruction = instructions[i],
      lines = current_lines
    )
    
    i <- j
  }
  
  # Sort by instruction order
  sorted <- character(0)
  
  for (instr in order) {
    matching <- which(sapply(grouped_lines, function(x) x$instruction == instr))
    
    for (idx in matching) {
      sorted <- c(sorted, grouped_lines[[idx]]$lines)
    }
  }
  
  # Add any instructions not in the specified order
  remaining <- which(!sapply(grouped_lines, function(x) x$instruction %in% order))
  for (idx in remaining) {
    sorted <- c(sorted, grouped_lines[[idx]]$lines)
  }
  
  dockerfile$lines <- sorted
  dockerfile
}
