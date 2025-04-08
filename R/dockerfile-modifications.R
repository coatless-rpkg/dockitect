#' Add a line to a `dockerfile` at a specific position
#'
#' Adds a raw line to a `dockerfile`` at a specified position. This is a lower-level
#' function typically used internally by higher-level functions.
#'
#' @param dockerfile A `dockerfile` object
#' @param line       Line to add (raw text)
#' @param after      Position after which to add the line (default: end of file)
#'
#' @return
#' An updated `dockerfile` object with the new line added
#'
#' @examples
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0")
#'   
#' # Add a comment after the FROM instruction
#' df <- dfm_add_line(df, "# This is a comment", after = 1)
#'
#' @details
#' Unlike the instruction-specific functions (`dfi_*`), this function adds
#' raw text without any formatting or validation. It's useful for adding
#' comments or custom instructions not covered by the built-in functions.
#'
#' @seealso
#' [dfm_remove_line()] for removing a line &
#' [dfm_replace_line()] for replacing a line
#'
#' @family dockerfile modification functions
#' @export
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

#' Remove a line from a `dockerfile`
#'
#' Removes a line at the specified position from a `dockerfile`.
#'
#' @param dockerfile A `dockerfile` object
#' @param line_num   Line number to remove
#'
#' @return
#' An updated `dockerfile` object with the specified line removed
#'
#' @examples
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_run("apt-get update") |>
#'   dfi_run("apt-get install -y libcurl4-openssl-dev")
#'   
#' # Remove the second RUN instruction (line 3)
#' df <- dfm_remove_line(df, 3)
#'
#' @seealso
#' [dfm_add_line()] for adding a line &
#' [dfm_replace_line()] for replacing a line
#'
#' @family dockerfile modification functions
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

#' Replace a line in a `dockerfile`
#'
#' Replaces a line at the specified position with new content.
#'
#' @param dockerfile A `dockerfile` object
#' @param line_num   Line number to replace
#' @param new_line   New line content
#'
#' @return
#' An updated `dockerfile` object with the specified line replaced
#'
#' @examples
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_run("apt-get update")
#'   
#' # Replace the RUN instruction with a more comprehensive one
#' df <- dfm_replace_line(df, 2, 
#'   "RUN apt-get update && apt-get install -y libcurl4-openssl-dev && apt-get clean")
#'
#' @seealso
#' [dfm_add_line()] for adding a line &
#' [dfm_remove_line()] for removing a line
#'
#' @family dockerfile modification functions
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

#' Move a line in a `dockerfile`
#'
#' Moves a line from one position to another in a `dockerfile`.
#'
#' @param dockerfile A `dockerfile` object
#' @param from       Source line number
#' @param to         Target position
#'
#' @return
#' An updated `dockerfile` object with the line moved to the new position
#'
#' @examples
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_workdir("/app") |>
#'   dfi_run("apt-get update") |>
#'   dfi_copy(".", "/app/")
#' 
#' df
#'   
#' # Move the RUN instruction to be after COPY
#' df <- dfm_move_line(df, 3, 4)
#' df
#' 
#' @details
#' This function allows for reorganizing instructions in a **Dockerfile** by moving
#' lines to different positions. It's useful for correcting the order of
#' instructions without having to recreate the entire **Dockerfile**.
#' 
#' Note that moving certain instructions to incompatible positions can make
#' the **Dockerfile** invalid (e.g., moving a `FROM` instruction after a `RUN`).
#' Consider using [dfm_sort_by_instruction()] to follow Docker best practices.
#'
#' @seealso
#' [dfm_add_line()] for adding a line,
#' [dfm_remove_line()] for removing a line, &
#' [dfm_sort_by_instruction()] for sorting instructions by type
#'
#' @family dockerfile modification functions
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

#' Group similar instructions in a `dockerfile`
#'
#' Optimizes a `dockerfile` by grouping similar consecutive instructions
#' into single multi-command instructions where appropriate. This can reduce
#' the number of layers in the final Docker image.
#'
#' @param dockerfile A `dockerfile` object
#'
#' @return
#' A new `dockerfile` object with similar instructions grouped
#'
#' @examples
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_run("apt-get update") |>
#'   dfi_run("apt-get install -y curl") |>
#'   dfi_run("apt-get clean")
#'   
#' # Group the three RUN instructions into one
#' df <- dfm_group_similar(df)
#'
#' @details
#' This function primarily targets `RUN` instructions, combining them with `&&`
#' to create single multi-command instructions. This follows Docker best practices
#' by reducing the number of layers in the final image. Instructions like `FROM`,
#' `WORKDIR`, `USER`, `ENTRYPOINT`, and `CMD` are left as separate instructions.
#'
#' @seealso
#' [dfm_sort_by_instruction()] for sorting instructions by type
#'
#' @family dockerfile modification functions
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

#' Sort instructions in a `dockerfile` by type
#'
#' Reorders the instructions in a `dockerfile` according to Docker best practices
#' or a custom order specification.
#'
#' @param dockerfile A `dockerfile` object
#' @param order      Custom order for instructions (optional)
#'
#' @return
#' A new `dockerfile` object with instructions sorted by type
#'
#' @examples
#' df <- dockerfile() |>
#'   dfi_cmd("R --no-save") |>
#'   dfi_run("apt-get update") |>
#'   dfi_from("rocker/r-ver:4.4.0")
#'   
#' # Sort according to best practices (FROM first, etc.)
#' df <- dfm_sort_by_instruction(df)
#'
#' # Use a custom order
#' df <- dfm_sort_by_instruction(df, 
#'   order = c("FROM", "RUN", "WORKDIR", "CMD"))
#'
#' @details
#' The default order follows Docker best practices, with instructions that change
#' less frequently appearing first (e.g., `FROM`, `ARG`, `LABEL`), and instructions
#' that change more frequently appearing later (e.g., `COPY`, `RUN`). This improves
#' caching and build performance.
#'
#' @seealso
#' [dfm_group_similar()] for grouping similar instructions
#'
#' @family dockerfile modification functions
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
