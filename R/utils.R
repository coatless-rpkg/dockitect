#' Detect the package manager from a base image
#'
#' @param base_image Base image name
#' @return Character string of package manager type ("apt", "yum", "apk", etc.)
#' @export
get_package_manager <- function(base_image) {
  if (is.null(base_image)) {
    return(NULL)
  }
  
  # Extract distribution name from base image
  parts <- strsplit(base_image, ":")[[1]][1]
  parts <- tolower(parts)  # Convert to lowercase
  
  # Check full image path before extracting basename
  if (grepl("rocker/", parts)) {
    return("apt")
  } else if (grepl("opensuse/|suse/", parts)) {
    return("zypper")
  }
  
  # Extract just the basename for other cases
  base_name <- basename(parts)  # Handle cases like "registry.com/ubuntu"
  
  if (grepl("ubuntu|debian", base_name)) {
    return("apt")
  } else if (grepl("centos|fedora|rhel|redhat", base_name)) {
    return("yum")
  } else if (grepl("alpine", base_name)) {
    return("apk")
  } else if (grepl("arch", base_name)) {
    return("pacman")
  } else {
    cli::cli_warn("Could not determine package manager for {base_image}. Defaulting to apt.")
    return("apt")
  }
}