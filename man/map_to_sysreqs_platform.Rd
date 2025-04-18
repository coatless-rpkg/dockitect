% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{map_to_sysreqs_platform}
\alias{map_to_sysreqs_platform}
\title{Map package manager to sysreqs platform}
\usage{
map_to_sysreqs_platform(package_manager, os = NULL)
}
\arguments{
\item{package_manager}{Package manager type (e.g., "apt", "yum")}

\item{os}{Operating system (if known)}
}
\value{
Character string of platform for sysreqs
}
\description{
Maps a package manager to the corresponding platform identifier used by
the pak package's system requirements feature.
}
\details{
This internal function maps package managers to platform identifiers
understood by the pak package's system requirements feature. It uses
the following mappings:
\itemize{
\item apt → ubuntu (or debian if specified)
\item yum → centos (or fedora, redhat, rockylinux if specified)
\item zypper → opensuse (or sle if specified)
\item apk → ubuntu (Alpine not directly supported)
\item pacman → ubuntu (Arch not directly supported)
}

If the OS is explicitly provided and supported by pak, that platform
is used directly.
}
\seealso{
\code{\link[=determine_package_manager]{determine_package_manager()}} for determining the package manager &
\code{\link[=dk_add_sysreqs]{dk_add_sysreqs()}} for adding system requirements to a dockerfile

Other utility functions: 
\code{\link{determine_linux_distribution}()},
\code{\link{determine_package_manager}()},
\code{\link{dk_add_sysreqs}()}
}
\concept{utility functions}
\keyword{internal}
