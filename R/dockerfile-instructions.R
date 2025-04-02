#' Add a `FROM` instruction to a `dockerfile`
#'
#' Adds a `FROM` instruction to specify the base image for the Docker build.
#' This is typically the first instruction in a **Dockerfile**.
#'
#' @param dockerfile A `dockerfile` object
#' @param image      Base image name (e.g., "rocker/r-ver:4.4.0")
#' @param as         Name for this build stage (optional, for multi-stage builds)
#'
#' @return
#' An updated `dockerfile` object with the `FROM` instruction added
#'
#' @examples
#' # Use a specific R version
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0")
#' df
#'
#' # Use a multi-stage build
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0", as = "build")
#' df
#' 
#' @details
#' The `FROM` instruction initializes a new build stage and sets the base image.
#' The metadata for the `dockerfile` object (package manager, OS, and R version)
#' is automatically updated based on the base image.
#'
#' @seealso
#' [dockerfile()] for creating a dockerfile object,
#' [dfi_run()] for adding commands to run in the container, & 
#' [Official Docker `FROM` documentation](https://docs.docker.com/engine/reference/builder/#from)
#'
#' @family dockerfile instruction functions
#' @export
dfi_from <- function(dockerfile, image, as = NULL) {
  check_dockerfile(dockerfile)
  
  args <- image
  if (!is.null(as)) {
    args <- paste0(args, " AS ", as)
  }
  
  add_dockerfile_line(dockerfile, "FROM", args)
}

#' Add a `RUN` instruction to a `dockerfile`
#'
#' Adds a `RUN` instruction to execute commands in a new layer on top of the
#' current image and commit the results.
#'
#' @param dockerfile A `dockerfile` object
#' @param commands   Commands to run (character vector for multiple commands)
#'
#' @return
#' An updated `dockerfile` object with the `RUN` instruction added
#'
#' @examples
#' # Single command
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_run("apt update")
#' df
#'
#' # Multiple commands in a single RUN (better practice)
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_run(c(
#'     "apt update",
#'     "apt install -y --no-install-recommends libcurl4-openssl-dev",
#'     "apt clean",
#'     "rm -rf /var/lib/apt/lists/*"
#'   ))
#'
#' @details
#' When providing multiple commands as a character vector, they will be
#' properly formatted with line continuations in the **Dockerfile**.
#' This is the recommended approach for package installations to reduce
#' the number of layers in the final image.
#'
#' @seealso
#' [dfi_from()] for setting the base image,
#' [dfi_cmd()] for specifying the default command to run, & 
#' [Official Docker `RUN` documentation](https://docs.docker.com/engine/reference/builder/#run)
#'
#' @family dockerfile instruction functions
#' @export
dfi_run <- function(dockerfile, commands) {
  check_dockerfile(dockerfile)
    
  # Join multiple commands with &&
  command_str <- paste(commands, collapse = " && ")
  add_dockerfile_line(dockerfile, "RUN", command_str)
}

#' Add a `COPY` instruction to a `dockerfile`
#'
#' Adds a `COPY` instruction to copy files or directories from the build context
#' to the container's filesystem.
#'
#' @param dockerfile A `dockerfile` object
#' @param src        Source path (relative to build context)
#' @param dest       Destination path (in the container)
#' @param from       Build stage to copy from (optional, for multi-stage builds)
#'
#' @return
#' An updated `dockerfile` object with the `COPY` instruction added
#'
#' @examples
#' # Copy a single file
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_copy("script.R", "/app/script.R")
#'
#' # Copy from a previous build stage
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0", as = "build") |>
#'   dfi_run("R -e \"install.packages('renv')\"") |>
#'   dfi_from("rocker/r-ver:4.4.0", as = "final") |>
#'   dfi_copy("/usr/local/lib/R/site-library/renv", 
#'           "/usr/local/lib/R/site-library/renv", 
#'           from = "build")
#'
#' @details
#' The `COPY` instruction copies new files or directories from `src`
#' and adds them to the filesystem of the container at the path `dest`.
#' When used with the `from` parameter, it can copy files from previous
#' build stages in multi-stage builds.
#'
#' @seealso
#' [dfi_add()] for similar functionality with additional features (like URL support),
#' [dfi_workdir()] for setting the working directory, & 
#' [Official Docker `COPY` documentation](https://docs.docker.com/engine/reference/builder/#copy)
#'
#' @family dockerfile instruction functions
#' @export
dfi_copy <- function(dockerfile, src, dest, from = NULL) {
  check_dockerfile(dockerfile)
  
  args <- paste(src, dest)
  if (!is.null(from)) {
    args <- paste0("--from=", from, " ", args)
  }
  
  add_dockerfile_line(dockerfile, "COPY", args)
}

#' Add a `WORKDIR` instruction to a `dockerfile`
#'
#' Adds a `WORKDIR` instruction to set the working directory for any subsequent
#' `RUN`, `CMD`, `ENTRYPOINT`, `COPY`, and `ADD` instructions.
#'
#' @param dockerfile A `dockerfile` object
#' @param path       Working directory path in the container
#'
#' @return
#' An updated `dockerfile` object with the `WORKDIR` instruction added
#'
#' @examples
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_workdir("/app") |>
#'   dfi_copy(".", "/app/")
#'
#' @details
#' The `WORKDIR` instruction can be used multiple times in a **Dockerfile**.
#' If a relative path is provided, it will be relative to the previous
#' `WORKDIR` instruction. If the directory doesn't exist, it will be created.
#'
#' @seealso
#' [dfi_copy()] for copying files into the container,
#' [dfi_run()] for executing commands in the working directory, &
#' [Official Docker `WORKDIR` documentation](https://docs.docker.com/engine/reference/builder/#workdir)
#'
#' @family dockerfile instruction functions
#' @export
dfi_workdir <- function(dockerfile, path) {
  check_dockerfile(dockerfile)
  add_dockerfile_line(dockerfile, "WORKDIR", path)
}

#' Add a `CMD` instruction to a `dockerfile`
#'
#' Adds a `CMD` instruction to provide defaults for an executing container.
#' These defaults can be executable programs or commands to be executed when
#' the container starts.
#'
#' @param dockerfile A `dockerfile` object
#' @param command    Command to run (character vector or string)
#'
#' @return
#' An updated `dockerfile` object with the `CMD` instruction added
#'
#' @examples
#' # Simple command
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_copy("script.R", "/app/script.R") |>
#'   dfi_cmd("Rscript /app/script.R")
#'
#' # Array format (recommended)
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_cmd(c("R", "--no-save"))
#'
#' @details
#' The function automatically converts the command to JSON array format, which
#' is the recommended approach for better signal handling. 
#' Only one `CMD` instruction is effective in a **Dockerfile**; if multiple are specified,
#' only the last one will take effect.
#'
#' @seealso
#' [dfi_entrypoint()] for defining the main executable of the container,
#' [dfi_run()] for executing commands during the build, &
#' [Official Docker `CMD` documentation](https://docs.docker.com/engine/reference/builder/#cmd)
#'
#' @family dockerfile instruction functions
#' @export
dfi_cmd <- function(dockerfile, command) {
  check_dockerfile(dockerfile)
  
  # Always use JSON array format for safer signal handling
  if (length(command) == 1 && !grepl("^\\[", command)) {
    # For single string command, convert to array by splitting on spaces
    command_parts <- strsplit(command, "\\s+")[[1]]
    cmd_json <- jsonlite::toJSON(command_parts)
    add_dockerfile_line(dockerfile, "CMD", cmd_json)
  } else if (length(command) > 1) {
    # For multiple elements, convert to JSON array
    cmd_json <- jsonlite::toJSON(command)
    add_dockerfile_line(dockerfile, "CMD", cmd_json)
  } else {
    # Probably it's already in JSON format or is some special case
    add_dockerfile_line(dockerfile, "CMD", command)
  }
}

#' Add an `ENV` instruction to a dockerfile
#'
#' Adds one or more `ENV` instructions to set environment variables in the container.
#'
#' @param dockerfile A `dockerfile` object
#' @param ... Named environment variables
#'
#' @return
#' An updated `dockerfile` object with ENV instructions added
#'
#' @examples
#' # Add a single environment variable
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_env(PATH = "/usr/local/bin:$PATH")
#' df
#'  
#' # Add multiple environment variables
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_env(
#'     DEBIAN_FRONTEND = "noninteractive",
#'     TZ = "America/Chicago",
#'     LANG = "en_US.UTF-8"
#'   )
#' df
#' 
#' @details
#' Environment variables set with `ENV` persist throughout the container's runtime.
#' Each variable is added as a separate `ENV` instruction in the **Dockerfile**,
#' making it easier to track changes in the Docker build history.
#'
#' @seealso
#' [dfi_arg()] for build-time variables &
#' [Official Docker `ENV` documentation](https://docs.docker.com/engine/reference/builder/#env)
#'
#' @family dockerfile instruction functions
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

#' Add an `EXPOSE` instruction to a `dockerfile`
#'
#' Adds an `EXPOSE` instruction to inform Docker that the container will listen 
#' on the specified network ports at runtime.
#'
#' @param dockerfile A `dockerfile` object
#' @param ports      Ports to expose (numeric or character vector)
#'
#' @return
#' An updated `dockerfile` object with the `EXPOSE` instruction added
#'
#' @examples
#' # Expose a single port
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_expose(8080)
#'   
#' # Expose multiple ports
#' df <- dockerfile() |>
#'   dfi_from("rocker/shiny:4.4.0") |>
#'   dfi_expose(c(3838, 8080))
#'
#' @details
#' The `EXPOSE` instruction does not actually publish the port. It functions
#' as documentation between the person who builds the image and the person who
#' runs the container, about which ports are intended to be published.
#' 
#' To actually publish the port when running the container, use the `-p` flag
#' in the `docker run` command.
#'
#' @seealso
#' [dk_template_shiny()] for a template that exposes Shiny ports,
#' [dk_template_plumber()] for a template that exposes Plumber API ports, & 
#' [Official Docker `EXPOSE` documentation](https://docs.docker.com/engine/reference/builder/#expose)
#'
#' @family dockerfile instruction functions
#' @export
dfi_expose <- function(dockerfile, ports) {
  check_dockerfile(dockerfile)
  
  # Handle multiple ports
  if (length(ports) > 1) {
    ports <- paste(ports, collapse = " ")
  }
  
  add_dockerfile_line(dockerfile, "EXPOSE", ports)
}

#' Add a `LABEL` instruction to a `dockerfile`
#'
#' Adds a `LABEL` instruction to include metadata in the Docker image.
#'
#' @param dockerfile A `dockerfile` object
#' @param ... Named labels as key-value pairs
#'
#' @return
#' An updated `dockerfile` object with the `LABEL` instruction added
#'
#' @examples
#' # Add a single label
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_label(maintainer = "user@example.com")
#' df
#' 
#' # Add multiple labels
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_label(
#'     maintainer = "user@example.com",
#'     version = "1.0.0",
#'     description = "Example R application",
#'     org.opencontainers.image.source = "https://github.com/user/repo"
#'   )
#' df
#'
#' @details
#' Labels are key-value pairs that add metadata to your image. They can be used
#' to organize images, record licensing information, annotate build information,
#' or help with image automation.
#' 
#' [Common label conventions](https://github.com/opencontainers/image-spec/blob/main/annotations.md) 
#' include:
#' * `maintainer`: The person responsible for the image
#' * `org.opencontainers.image.authors`: Image authors
#' * `org.opencontainers.image.version`: Version of the packaged software
#' * `org.opencontainers.image.source`: URL to the source code
#'
#' @seealso
#' [dfi_maintainer()] for the deprecated maintainer instruction &
#' [Official Docker `LABEL` documentation](https://docs.docker.com/engine/reference/builder/#label)
#'
#' @family dockerfile instruction functions
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

#' Add an `ADD` instruction to a `dockerfile`
#'
#' Adds an `ADD` instruction to copy files, directories, or remote files 
#' from source to the container's filesystem at destination.
#'
#' @param dockerfile A `dockerfile` object
#' @param src        Source path (can be a URL or tar archive)
#' @param dest       Destination path in the container
#'
#' @return
#' An updated `dockerfile` object with the `ADD` instruction added
#'
#' @examples
#' # Add a local file
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_add("local.txt", "/app/local.txt")
#'   
#' # Add a file from a URL
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_add("https://example.com/file.txt", "/app/file.txt")
#'
#' @details
#' `ADD` is similar to `COPY`, but with additional features:
#' 
#' * If `src` is a URL, the file is downloaded from the URL
#' * If `src` is a local tar archive, it will be automatically unpacked
#' 
#' Note that `COPY` is generally preferred for simple file copying because
#' it's more explicit and has fewer side effects than `ADD`.
#'
#' @seealso
#' [dfi_copy()] for simpler file copying (generally preferred) &
#' [Official Docker `ADD` documentation](https://docs.docker.com/engine/reference/builder/#add)
#'
#' @family dockerfile instruction functions
#' @export
dfi_add <- function(dockerfile, src, dest) {
  check_dockerfile(dockerfile)
  add_dockerfile_line(dockerfile, "ADD", paste(src, dest))
}

#' Add an `ENTRYPOINT` instruction to a `dockerfile`
#'
#' Adds an `ENTRYPOINT` instruction to configure a container that will run as an executable.
#'
#' @param dockerfile A `dockerfile` object
#' @param entrypoint Entrypoint command (character vector or string)
#'
#' @return
#' An updated `dockerfile` object with the `ENTRYPOINT` instruction added
#'
#' @examples
#' # Simple entrypoint
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_entrypoint("R")
#' df
#'    
#' # Array format (recommended)
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_entrypoint(c("R", "--no-save"))
#' df
#' 
#' @details
#' The `ENTRYPOINT` instruction defines the executable that will be run when
#' the container starts. Any command line arguments passed to `docker run`
#' will be appended to the entrypoint command.
#' 
#' When used together with `CMD`, the `CMD` instruction provides default arguments
#' to the `ENTRYPOINT` command that can be overridden at runtime.
#' 
#' The function automatically converts the command to JSON array format if
#' a vector is provided, which is the recommended approach for proper signal handling.
#'
#' @seealso
#' [dfi_cmd()] for providing default arguments to the entrypoint &
#' [Official Docker `ENTRYPOINT` documentation](https://docs.docker.com/engine/reference/builder/#entrypoint)
#'
#' @family dockerfile instruction functions
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

#' Add a `USER` instruction to a `dockerfile`
#'
#' Adds a `USER` instruction to set the user or UID to use when running 
#' subsequent instructions and the default user for the container.
#'
#' @param dockerfile A `dockerfile` object
#' @param user       Username or UID
#' @param group      Group or GID (optional)
#'
#' @return
#' An updated `dockerfile` object with the USER instruction added
#'
#' @examples
#' # Set user by name
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_user("r-user")
#' df
#'   
#' # Set user and group by ID
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_user(1000, 1000)
#' df 
#' 
#' @details
#' The `USER` instruction sets the user and optionally the user group for
#' subsequent instructions in the **Dockerfile**, and as the default user for
#' running the container. This is important for security, as it's a best
#' practice to run containers with non-root users when possible.
#' 
#' If the user specified does not exist in the container, you'll need to create
#' it first using a `RUN` instruction.
#'
#' @seealso
#' [dfi_run()] for creating users &
#' [Official Docker `USER` documentation](https://docs.docker.com/engine/reference/builder/#user)
#'
#' @family dockerfile instruction functions
#' @export
dfi_user <- function(dockerfile, user, group = NULL) {
  check_dockerfile(dockerfile)
  
  if (!is.null(group)) {
    user <- paste0(user, ":", group)
  }
  
  add_dockerfile_line(dockerfile, "USER", user)
}

#' Add a `VOLUME` instruction to a `dockerfile`
#'
#' Adds a `VOLUME` instruction to create a mount point with the specified name
#' and mark it as holding externally mounted volumes.
#'
#' @param dockerfile A `dockerfile` object
#' @param paths      Path(s) to create as volumes
#'
#' @return
#' An updated `dockerfile` object with the `VOLUME` instruction added
#'
#' @examples
#' # Create a single volume
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_volume("/data")
#' df 
#'    
#' # Create multiple volumes
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_volume(c("/data", "/output", "/config"))
#' df
#' 
#' @details
#' The `VOLUME` instruction creates a mount point and marks it as being externally
#' mounted. This is useful for:
#' 
#' * Persistent data that should survive container restarts
#' * Data you want to share between containers
#' * Data you want to access from the host
#' 
#' Note that the actual binding of host directories happens at runtime using the
#' `-v` flag with `docker run`, not during the image build.
#'
#' @seealso
#' [dk_template_base()] for a template that sets up common volume patterns &
#' [Official Docker `VOLUME` documentation](https://docs.docker.com/engine/reference/builder/#volume)
#'
#' @family dockerfile instruction functions
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

#' Add an `ARG` instruction to a `dockerfile`
#'
#' Adds an `ARG` instruction to define a variable that users can pass at build-time
#' to the builder using the `--build-arg` flag.
#'
#' @param dockerfile A `dockerfile` object
#' @param name       Argument name
#' @param default    Default value (optional)
#'
#' @return
#' An updated `dockerfile` object with the `ARG` instruction added
#'
#' @examples
#' # Define an argument with no default
#' df <- dockerfile() |>
#'   dfi_arg("R_VERSION") |>
#'   dfi_from(paste0("rocker/r-ver:", "$R_VERSION"))
#' df
#'    
#' # Define an argument with a default value
#' df <- dockerfile() |>
#'   dfi_arg("R_VERSION", "4.4.0") |>
#'   dfi_from(paste0("rocker/r-ver:", "$R_VERSION"))
#' df
#' 
#' @details
#' Build arguments are only available during the build of a Docker image and
#' not when a container is running. They can be used to parameterize the build
#' process, allowing users to specify values like versions or configuration
#' options at build time.
#' 
#' When building the image, use:
#' 
#' ```bash
#' docker build --build-arg R_VERSION=4.4.0 -t my-image .
#' ```
#'
#' @seealso
#' [dfi_env()] for runtime environment variables &
#' [Official Docker `ARG` documentation](https://docs.docker.com/engine/reference/builder/#arg)
#'
#' @family dockerfile instruction functions
#' @export
dfi_arg <- function(dockerfile, name, default = NULL) {
  check_dockerfile(dockerfile)
  
  arg_str <- name
  if (!is.null(default)) {
    arg_str <- paste0(name, "=", default)
  }
  
  add_dockerfile_line(dockerfile, "ARG", arg_str)
}

#' Add a `HEALTHCHECK` instruction to a `dockerfile`
#'
#' Adds a `HEALTHCHECK` instruction to tell Docker how to test if a container is
#' still working properly.
#'
#' @param dockerfile   A `dockerfile` object
#' @param command      Health check command
#' @param interval     Interval between checks (e.g., "30s")
#' @param timeout      Timeout for checks (e.g., "5s")
#' @param start_period Initial grace period (e.g., "5s")
#' @param retries      Number of retries
#'
#' @return
#' An updated `dockerfile` object with the `HEALTHCHECK` instruction added
#'
#' @examples
#' # Simple health check using curl
#' df <- dockerfile() |>
#'   dfi_from("rocker/shiny:4.4.0") |>
#'   dfi_healthcheck("curl -f http://localhost:3838/ || exit 1",
#'                    interval = "30s",
#'                    timeout = "10s",
#'                    retries = 3)
#' df
#' 
#' @details
#' The `HEALTHCHECK` instruction tells Docker how to determine if a container
#' is healthy. The command should exit with 0 if the container is healthy, or
#' with 1 if it's unhealthy. This is particularly useful for web applications
#' or services where you want to ensure they're running correctly.
#' 
#' Parameter meanings:
#' 
#' * `interval`: How often to run the check (default: 30s)
#' * `timeout`: Maximum time the check can take (default: 30s)
#' * `start-period`: Grace period before counting retries (default: 0s)
#' * `retries`: Number of consecutive failures needed to mark unhealthy (default: 3)
#'
#' @seealso
#' [dk_template_shiny()] for a template for Shiny apps,
#' [dk_template_plumber()] for a template for Plumber APIs, &
#' [Official Docker `HEALTHCHECK` documentation](https://docs.docker.com/engine/reference/builder/#healthcheck)
#'
#' @family dockerfile instruction functions
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

#' Add a `SHELL` instruction to a `dockerfile`
#'
#' Adds a `SHELL` instruction to override the default shell used for commands.
#'
#' @param dockerfile A `dockerfile` object
#' @param shell      Shell command and parameters (character vector or string)
#'
#' @return
#' An updated `dockerfile` object with the `SHELL` instruction added
#'
#' @examples
#' # Set shell to bash with expanded error messaging
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_shell(c("/bin/bash", "-e", "-o", "pipefail", "-c"))
#' df
#' 
#' @details
#' The `SHELL` instruction allows overriding the default shell used for the
#' shell form of commands. The default shell on Linux is `["/bin/sh", "-c"]`,
#' and on Windows is `["cmd", "/S", "/C"]`.
#' 
#' This instruction is particularly useful for:
#' 
#' * Using bash-specific features
#' * Enabling better error handling with options like `-e` (exit on error)
#' * Setting up pipefail to catch errors in pipelines
#' 
#' The function automatically converts the shell command to JSON array format
#' if provided as a vector.
#'
#' @seealso
#' [dfi_run()] for executing commands with the shell &
#' [Official Docker `SHELL` documentation](https://docs.docker.com/engine/reference/builder/#shell)
#'
#' @family dockerfile instruction functions
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

#' Add a `STOPSIGNAL` instruction to a `dockerfile`
#'
#' Adds a `STOPSIGNAL` instruction to set the system call signal that will be
#' sent to the container to exit.
#'
#' @param dockerfile A `dockerfile` object
#' @param signal     Signal for stopping container (e.g., "SIGTERM", "9")
#'
#' @return
#' An updated `dockerfile` object with the `STOPSIGNAL` instruction added
#'
#' @examples
#' # Set SIGTERM as the stop signal
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_stopsignal("SIGTERM")
#' df
#'    
#' # Set using signal number
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_stopsignal("15")
#' df
#' 
#' @details
#' The `STOPSIGNAL` instruction sets the system call signal that will be sent to
#' the container to request it to exit. The signal can be specified as a signal name
#' in the format `SIGNAME` (e.g., `SIGTERM`), or as an unsigned number (e.g., `15`).
#' 
#' By default, Docker sends `SIGTERM` to containers when they need to be stopped.
#' If the container doesn't exit within the timeout period (default 10 seconds),
#' Docker sends `SIGKILL` to forcibly terminate it.
#'
#' @seealso
#' [dfi_healthcheck()] for configuring container health checks &
#' [Official Docker `STOPSIGNAL` documentation](https://docs.docker.com/engine/reference/builder/#stopsignal)
#'
#' @family dockerfile instruction functions
#' @export
dfi_stopsignal <- function(dockerfile, signal) {
  check_dockerfile(dockerfile)
  add_dockerfile_line(dockerfile, "STOPSIGNAL", signal)
}

#' Add a `MAINTAINER` instruction to a `dockerfile` (deprecated)
#'
#' Adds a `MAINTAINER` instruction to specify the author of the image.
#' This instruction is deprecated in favor of using `LABEL maintainer=...`.
#'
#' @param dockerfile A `dockerfile` object
#' @param maintainer Maintainer info (e.g., "Name <email@example.com>")
#'
#' @return
#' An updated `dockerfile` object with the `MAINTAINER` instruction added
#'
#' @examples
#' # Using the deprecated MAINTAINER instruction
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_maintainer("John Doe <john@example.com>")
#' df
#'    
#' # Better approach using LABEL
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_label(maintainer = "John Doe <john@example.com>")
#' df
#' 
#' @details
#' The `MAINTAINER` instruction has been deprecated since Docker 1.13.0 (2017)
#' in favor of using `LABEL maintainer=...`. This function is provided for
#' compatibility with older Dockerfiles, but new **Dockerfile**s should use
#' `dfi_label(maintainer = ...)` instead.
#'
#' @seealso
#' [dfi_label()] for the recommended way to specify the maintainer &
#' [Official Docker deprecated `MAINTAINER` documentation](https://docs.docker.com/engine/reference/builder/#maintainer-deprecated)
#'
#' @family dockerfile instruction functions
#' @export
dfi_maintainer <- function(dockerfile, maintainer) {
  check_dockerfile(dockerfile)
  cli::cli_warn("MAINTAINER is deprecated. Use {.code dfi_label(maintainer = ...)} instead.")
  add_dockerfile_line(dockerfile, "MAINTAINER", maintainer)
}

#' Add an `ONBUILD` instruction to a `dockerfile`
#'
#' Adds an `ONBUILD` instruction to register trigger instructions to be
#' executed later, when the image is used as the base for another build.
#'
#' @param dockerfile  A `dockerfile` object
#' @param instruction Instruction to run on build (without the instruction name)
#'
#' @return
#' An updated `dockerfile` object with the `ONBUILD` instruction added
#'
#' @examples
#' # Add ONBUILD triggers for package installation
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0") |>
#'   dfi_onbuild("COPY renv.lock /app/renv.lock") |>
#'   dfi_onbuild("RUN R -e \"renv::restore()\"")
#' df
#' 
#' @details
#' The `ONBUILD` instruction registers a trigger instruction that will be 
#' executed when the image is used as the base for another build. The trigger
#' is executed in the context of the downstream build, as if it had been
#' inserted immediately after the `FROM` instruction in the downstream **Dockerfile**.
#' 
#' This is useful for creating "builder" images that can set up a common build
#' environment for applications.
#' 
#' Note: `ONBUILD` instructions are not inherited by "grand-children" builds.
#'
#' @seealso
#' [dfi_from()] for specifying the base image &
#' [Official Docker `ONBUILD` documentation](https://docs.docker.com/engine/reference/builder/#onbuild)
#'
#' @family dockerfile instruction functions
#' @export
dfi_onbuild <- function(dockerfile, instruction) {
  check_dockerfile(dockerfile)
  add_dockerfile_line(dockerfile, "ONBUILD", instruction)
}
