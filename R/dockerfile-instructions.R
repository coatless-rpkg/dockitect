#' Add a FROM instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param image Base image name
#' @param as Name for this build stage (optional)
#' @return Updated dockerfile object
#' @export
dfi_from <- function(dockerfile, image, as = NULL) {
  check_dockerfile(dockerfile)
  
  args <- image
  if (!is.null(as)) {
    args <- paste0(args, " AS ", as)
  }
  
  add_dockerfile_line(dockerfile, "FROM", args)
}

#' Add a RUN instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param commands Commands to run
#' @return Updated dockerfile object
#' @export
dfi_run <- function(dockerfile, commands) {
  check_dockerfile(dockerfile)
  add_dockerfile_line(dockerfile, "RUN", commands)
}

#' Add a COPY instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param src Source path
#' @param dest Destination path
#' @param from Build stage to copy from (optional)
#' @return Updated dockerfile object
#' @export
dfi_copy <- function(dockerfile, src, dest, from = NULL) {
  check_dockerfile(dockerfile)
  
  args <- paste(src, dest)
  if (!is.null(from)) {
    args <- paste0("--from=", from, " ", args)
  }
  
  add_dockerfile_line(dockerfile, "COPY", args)
}

#' Add a WORKDIR instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param path Working directory path
#' @return Updated dockerfile object
#' @export
dfi_workdir <- function(dockerfile, path) {
  check_dockerfile(dockerfile)
  add_dockerfile_line(dockerfile, "WORKDIR", path)
}

#' Add a CMD instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param command Command to run
#' @return Updated dockerfile object
#' @export
dfi_cmd <- function(dockerfile, command) {
  check_dockerfile(dockerfile)
  
  # Handle vector input for JSON array format
  if (length(command) > 1) {
    cmd_json <- jsonlite::toJSON(command)
    add_dockerfile_line(dockerfile, "CMD", cmd_json)
  } else {
    add_dockerfile_line(dockerfile, "CMD", command)
  }
}

#' Add an ENV instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param ... Named environment variables
#' @return Updated dockerfile object
#' @export
dfi_env <- function(dockerfile, ...) {
  check_dockerfile(dockerfile)
  
  env_vars <- list(...)
  if (length(env_vars) == 0) {
    return(dockerfile)
  }
  
  # Process each environment variable
  for (var_name in names(env_vars)) {
    var_value <- env_vars[[var_name]]
    dockerfile <- add_dockerfile_line(dockerfile, "ENV", paste(var_name, var_value))
  }
  
  dockerfile
}

#' Add an EXPOSE instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param ports Ports to expose (numeric or character)
#' @return Updated dockerfile object
#' @export
dfi_expose <- function(dockerfile, ports) {
  check_dockerfile(dockerfile)
  
  # Handle multiple ports
  if (length(ports) > 1) {
    ports <- paste(ports, collapse = " ")
  }
  
  add_dockerfile_line(dockerfile, "EXPOSE", ports)
}

#' Add a LABEL instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param ... Named labels
#' @return Updated dockerfile object
#' @export
dfi_label <- function(dockerfile, ...) {
  check_dockerfile(dockerfile)
  
  labels <- list(...)
  if (length(labels) == 0) {
    return(dockerfile)
  }
  
  # Build label string
  label_parts <- character(length(labels))
  for (i in seq_along(labels)) {
    key <- names(labels)[i]
    value <- labels[[i]]
    # Escape quotes in values
    value <- gsub('"', '\\"', value)
    label_parts[i] <- paste0(key, '="', value, '"')
  }
  
  label_str <- paste(label_parts, collapse = " ")
  add_dockerfile_line(dockerfile, "LABEL", label_str)
}

#' Add an ADD instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param src Source path (can be a URL)
#' @param dest Destination path
#' @return Updated dockerfile object
#' @export
dfi_add <- function(dockerfile, src, dest) {
  check_dockerfile(dockerfile)
  add_dockerfile_line(dockerfile, "ADD", paste(src, dest))
}

#' Add an ENTRYPOINT instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param entrypoint Entrypoint command
#' @return Updated dockerfile object
#' @export
dfi_entrypoint <- function(dockerfile, entrypoint) {
  check_dockerfile(dockerfile)
  
  # Handle vector input for JSON array format
  if (length(entrypoint) > 1) {
    cmd_json <- jsonlite::toJSON(entrypoint)
    add_dockerfile_line(dockerfile, "ENTRYPOINT", cmd_json)
  } else {
    add_dockerfile_line(dockerfile, "ENTRYPOINT", entrypoint)
  }
}

#' Add a USER instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param user Username or UID
#' @param group Group or GID (optional)
#' @return Updated dockerfile object
#' @export
dfi_user <- function(dockerfile, user, group = NULL) {
  check_dockerfile(dockerfile)
  
  if (!is.null(group)) {
    user <- paste0(user, ":", group)
  }
  
  add_dockerfile_line(dockerfile, "USER", user)
}

#' Add a VOLUME instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param paths Path(s) to create as volumes
#' @return Updated dockerfile object
#' @export
dfi_volume <- function(dockerfile, paths) {
  check_dockerfile(dockerfile)
  
  # Handle multiple paths
  if (length(paths) > 1) {
    vol_json <- jsonlite::toJSON(paths)
    add_dockerfile_line(dockerfile, "VOLUME", vol_json)
  } else {
    add_dockerfile_line(dockerfile, "VOLUME", paths)
  }
}

#' Add an ARG instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param name Argument name
#' @param default Default value (optional)
#' @return Updated dockerfile object
#' @export
dfi_arg <- function(dockerfile, name, default = NULL) {
  check_dockerfile(dockerfile)
  
  arg_str <- name
  if (!is.null(default)) {
    arg_str <- paste0(name, "=", default)
  }
  
  add_dockerfile_line(dockerfile, "ARG", arg_str)
}

#' Add a HEALTHCHECK instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param command Health check command
#' @param interval Interval between checks (e.g., "30s")
#' @param timeout Timeout for checks (e.g., "5s")
#' @param start_period Initial grace period (e.g., "5s")
#' @param retries Number of retries
#' @return Updated dockerfile object
#' @export
dfi_healthcheck <- function(dockerfile, command, interval = NULL, timeout = NULL, 
                            start_period = NULL, retries = NULL) {
  check_dockerfile(dockerfile)
  
  options <- character()
  
  if (!is.null(interval)) {
    options <- c(options, paste0("--interval=", interval))
  }
  if (!is.null(timeout)) {
    options <- c(options, paste0("--timeout=", timeout))
  }
  if (!is.null(start_period)) {
    options <- c(options, paste0("--start-period=", start_period))
  }
  if (!is.null(retries)) {
    options <- c(options, paste0("--retries=", retries))
  }
  
  args <- paste(c(options, paste("CMD", command)), collapse = " ")
  add_dockerfile_line(dockerfile, "HEALTHCHECK", args)
}

#' Add a SHELL instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param shell Shell command and parameters
#' @return Updated dockerfile object
#' @export
dfi_shell <- function(dockerfile, shell) {
  check_dockerfile(dockerfile)
  
  # Convert to JSON array format if not already
  if (length(shell) > 1 || !grepl("^\\[", shell)) {
    if (length(shell) == 1) {
      shell <- strsplit(shell, " ")[[1]]
    }
    shell <- jsonlite::toJSON(shell)
  }
  
  add_dockerfile_line(dockerfile, "SHELL", shell)
}

#' Add a STOPSIGNAL instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param signal Signal for stopping container
#' @return Updated dockerfile object
#' @export
dfi_stopsignal <- function(dockerfile, signal) {
  check_dockerfile(dockerfile)
  add_dockerfile_line(dockerfile, "STOPSIGNAL", signal)
}

#' Add a MAINTAINER instruction to a dockerfile (deprecated)
#'
#' @param dockerfile A dockerfile object
#' @param maintainer Maintainer info
#' @return Updated dockerfile object
#' @export
dfi_maintainer <- function(dockerfile, maintainer) {
  check_dockerfile(dockerfile)
  cli::cli_warn("MAINTAINER is deprecated. Use {.code dfi_label(maintainer = ...)} instead.")
  add_dockerfile_line(dockerfile, "MAINTAINER", maintainer)
}

#' Add an ONBUILD instruction to a dockerfile
#'
#' @param dockerfile A dockerfile object
#' @param instruction Instruction to run on build
#' @return Updated dockerfile object
#' @export
dfi_onbuild <- function(dockerfile, instruction) {
  check_dockerfile(dockerfile)
  add_dockerfile_line(dockerfile, "ONBUILD", instruction)
}