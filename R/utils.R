#' Determine the Linux Distribution from a base image
#'
#' Analyzes a Docker base image name to determine the underlying distribution
#'
#' @param base_image Base image name
#'
#' @return
#' Character string of OS type (e.g., "ubuntu", "centos", "debian", "alpine")
#'
#' @examples
#' determine_linux_distribution("rocker/r-ver:4.4.0")  # Returns "ubuntu"
#' determine_linux_distribution("alpine:3.16")         # Returns "alpine"
#' determine_linux_distribution("centos:7")            # Returns "centos"
#'
#' @details
#' This function parses the base image name to extract the underlying
#' distribution. For Rocker Project images (which are based on Ubuntu/Debian),
#' it returns "ubuntu". For other distributions, it attempts to identify
#' common ones like Debian, CentOS, Fedora, Alpine, etc.
#'
#' @seealso
#' [determine_package_manager()] for determining the package manager &
#' [map_to_sysreqs_platform()] for mapping to sysreqs platform
#'
#' @family utility functions
#' @export
determine_linux_distribution <- function(base_image) {
  if (is.null(base_image)) {
    return(NULL)
  }
  
  # Extract distribution name from base image
  parts <- strsplit(base_image, ":")[[1]][1]
  parts <- tolower(parts)  # Convert to lowercase
  
  # Check for rocker images (based on debian/ubuntu)
  if (grepl("rocker/", parts)) {
    return("ubuntu")
  }
  
  # Check for specific distros
  if (grepl("ubuntu", parts)) {
    return("ubuntu")
  } else if (grepl("debian", parts)) {
    return("debian")
  } else if (grepl("centos", parts)) {
    return("centos")
  } else if (grepl("fedora", parts)) {
    return("fedora")
  } else if (grepl("redhat|rhel", parts)) {
    return("redhat")
  } else if (grepl("rocky|rockylinux", parts)) {
    return("rockylinux")
  } else if (grepl("opensuse", parts)) {
    return("opensuse")
  } else if (grepl("suse/sle|sles", parts)) {
    return("sle")
  } else if (grepl("alpine", parts)) {
    return("alpine")
  } else if (grepl("arch", parts)) {
    return("arch")
  } else {
    # Default to ubuntu if we can't determine
    cli::cli_warn("Could not determine OS for {base_image}. Defaulting to ubuntu.")
    return("ubuntu")
  }
}

#' Determine the package manager from a base image
#'
#' Analyzes a Docker base image name to determine the appropriate package manager.
#'
#' @param base_image Base image name
#'
#' @return
#' Character string of package manager type ("apt", "yum", "apk", "zypper", "pacman")
#'
#' @examples
#' determine_package_manager("rocker/r-ver:4.4.0")  # Returns "apt"
#' determine_package_manager("alpine:3.16")         # Returns "apk"
#' determine_package_manager("centos:7")            # Returns "yum"
#'
#' @details
#' This function first identifies the linux distribution using 
#' [determine_linux_distribution()], then maps that to the appropriate
#' package manager:
#' 
#' * Ubuntu/Debian → apt
#' * CentOS/Fedora/RHEL → yum
#' * Alpine → apk
#' * OpenSUSE → zypper
#' * Arch → pacman
#'
#' @seealso
#' [determine_linux_distribution()] for determining the operating system &
#' [generate_pkg_install_cmd()] for generating package installation commands
#'
#' @family utility functions
#' @export
determine_package_manager <- function(base_image) {
  if (is.null(base_image)) {
    return(NULL)
  }
  
  # Get the distribution first
  os <- determine_linux_distribution(base_image)
  
  # Map distribution to package manager
  if (os %in% c("ubuntu", "debian")) {
    return("apt")
  } else if (os %in% c("centos", "fedora", "redhat", "rockylinux")) {
    return("yum")
  } else if (os == "alpine") {
    return("apk")
  } else if (os %in% c("opensuse", "sle")) {
    return("zypper")
  } else if (os == "arch") {
    return("pacman")
  } else {
    cli::cli_warn("Could not determine package manager for {base_image}. Defaulting to apt.")
    return("apt")
  }
}

#' Map package manager to sysreqs platform
#'
#' Maps a package manager to the corresponding platform identifier used by
#' the pak package's system requirements feature.
#'
#' @param package_manager Package manager type (e.g., "apt", "yum")
#' @param os Operating system (if known)
#'
#' @return
#' Character string of platform for sysreqs
#'
#' @details
#' This internal function maps package managers to platform identifiers
#' understood by the pak package's system requirements feature. It uses
#' the following mappings:
#' 
#' - apt → ubuntu (or debian if specified)
#' - yum → centos (or fedora, redhat, rockylinux if specified)
#' - zypper → opensuse (or sle if specified)
#' - apk → ubuntu (Alpine not directly supported)
#' - pacman → ubuntu (Arch not directly supported)
#' 
#' If the OS is explicitly provided and supported by pak, that platform
#' is used directly.
#'
#' @seealso
#' [determine_package_manager()] for determining the package manager &
#' [dk_add_sysreqs()] for adding system requirements to a dockerfile
#'
#' @family utility functions
#' @keywords internal
map_to_sysreqs_platform <- function(package_manager, os = NULL) {
  if (!is.null(os)) {
    # If OS is explicitly provided, use it if supported
    if (os %in% c("ubuntu", "debian", "centos", "fedora", "opensuse", 
                  "redhat", "rockylinux", "sle")) {
      return(os)
    }
  }
  
  # Map from package manager to a supported platform
  switch(package_manager,
         "apt" = "ubuntu",
         "yum" = "centos",
         "zypper" = "opensuse",
         "apk" = "ubuntu",  # No direct Alpine support, fall back to Ubuntu
         "pacman" = "ubuntu", # No direct Arch support, fall back to Ubuntu
         "ubuntu") # Default to Ubuntu
}


#' Generate package installation commands for different package managers
#'
#' @param package_manager Character string specifying the package manager to use
#' @param packages Character vector of packages to install
#' @return Character vector of installation commands
#' @export
generate_pkg_install_cmd <- function(package_manager, packages) {
  if (length(packages) == 0) {
    return(character(0))
  }
  
  # Ensure packages is not NULL and has length > 0
  if (is.null(packages) || length(packages) == 0) {
    return(character(0))
  }
  
  packages_str <- paste(packages, collapse = " ")
  
  switch(package_manager,
         "apt" = {
           c(
             "apt-get update -y",
             paste0("apt-get install -y --no-install-recommends ", packages_str),
             "apt-get clean",
             "rm -rf /var/lib/apt/lists/*"
           )
         },
         "yum" = {
           c(
             "yum update -y",
             paste0("yum install -y ", packages_str),
             "yum clean all"
           )
         },
         "apk" = {
           c(
             "apk update",
             paste0("apk add --no-cache ", packages_str)
           )
         },
         "zypper" = {
           c(
             "zypper refresh",
             paste0("zypper install -y ", packages_str),
             "zypper clean"
           )
         },
         "pacman" = {
           c(
             paste0("pacman -Sy --noconfirm ", packages_str)
           )
         },
         {
           cli::cli_warn("Unsupported package manager: {package_manager}. Using apt commands.")
           c(
             "apt-get update -y",
             paste0("apt-get install -y --no-install-recommends ", packages_str),
             "apt-get clean",
             "rm -rf /var/lib/apt/lists/*"
           )
         }
  )
}



#' Add system requirements for R packages to a `dockerfile`
#'
#' Identifies and adds the necessary system requirements for R packages 
#' to a `dockerfile` using the `pak` package to determine dependencies.
#'
#' @param dockerfile      A `dockerfile` object
#' @param packages        Character vector of package names
#' @param package_manager Package manager to use (default: `"auto"`)
#'
#' @return
#' An updated `dockerfile` object with system requirements added
#'
#' @examples
#' \dontrun{
#' # Create a dockerfile
#' df <- dockerfile() |>
#'   dfi_from("rocker/r-ver:4.4.0")
#' 
#' # Add system requirements for packages
#' df <- dk_add_sysreqs(df, c("xml2", "RPostgreSQL", "rJava"))
#' }
#'
#' @details
#' This function uses the pak package to determine the system requirements
#' for the specified R packages. It then formats the appropriate installation
#' commands for the detected package manager and adds them as `RUN` instructions
#' to the `dockerfile`.
#'
#' The `pak` package must be installed for this function to work.
#'
#' @seealso
#' [generate_pkg_install_cmd()] for generating package installation commands &
#' [determine_package_manager()] for determining the package manager
#'
#' @family utility functions
#' @export
dk_add_sysreqs <- function(dockerfile, packages, package_manager = "auto") {
  check_dockerfile(dockerfile)
  
  if (package_manager == "auto") {
    package_manager <- dockerfile$metadata$package_manager
    if (is.null(package_manager)) {
      package_manager <- "apt"
      cli::cli_warn("Could not determine package manager. Defaulting to apt.")
    }
  }
  
  # Get system requirements using pak
  if (!requireNamespace("pak", quietly = TRUE)) {
    cli::cli_warn("Package 'pak' is required to determine system requirements. Skipping.")
    return(dockerfile)
  }
  
  # Map package manager to appropriate sysreqs platform
  os <- determine_linux_distribution(dockerfile$metadata$base_image)
  platform <- map_to_sysreqs_platform(package_manager, os)
  
  # Get system requirements data frame
  sysreqs_df <- pak::pkg_sysreqs(packages, sysreqs_platform = platform)
  
  if (is.null(sysreqs_df$packages) || nrow(sysreqs_df$packages) == 0) {
    return(dockerfile)
  }
  
  # Extract system packages from the data frame and remove duplicates
  system_packages <- unique(unlist(sysreqs_df$packages$system_packages))
  
  if (length(system_packages) == 0) {
    return(dockerfile)
  }
  
  # Generate system-specific install commands using utility function
  install_cmd <- generate_pkg_install_cmd(package_manager, system_packages)
  
  # Add the installation command
  dockerfile <- dfi_run(dockerfile, paste(install_cmd, collapse = " && "))
  
  dockerfile
}
