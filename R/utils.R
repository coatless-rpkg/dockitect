#' Determine the operating system from a base image
#'
#' @param base_image Base image name
#' @return Character string of OS type (e.g., "ubuntu", "centos", "debian")
#' @export
determine_os <- function(base_image) {
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
#' @param base_image Base image name
#' @return Character string of package manager type ("apt", "yum", "apk", etc.)
#' @export
determine_package_manager <- function(base_image) {
  if (is.null(base_image)) {
    return(NULL)
  }
  
  # Get the OS first
  os <- determine_os(base_image)
  
  # Map OS to package manager
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
#' @param package_manager Package manager type
#' @param os Operating system (if known)
#' @return Character string of platform for sysreqs
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