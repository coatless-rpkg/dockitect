% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dockerfile-instructions.R
\name{dfi_env}
\alias{dfi_env}
\title{Add an \code{ENV} instruction to a dockerfile}
\usage{
dfi_env(dockerfile, ...)
}
\arguments{
\item{dockerfile}{A \code{dockerfile} object}

\item{...}{Named environment variables}
}
\value{
An updated \code{dockerfile} object with ENV instructions added
}
\description{
Adds one or more \code{ENV} instructions to set environment variables in the container.
}
\details{
Environment variables set with \code{ENV} persist throughout the container's runtime.
Each variable is added as a separate \code{ENV} instruction in the \strong{Dockerfile},
making it easier to track changes in the Docker build history.
}
\examples{
# Add a single environment variable
df <- dockerfile() |>
  dfi_from("rocker/r-ver:4.4.0") |>
  dfi_env(PATH = "/usr/local/bin:$PATH")
df
 
# Add multiple environment variables
df <- dockerfile() |>
  dfi_from("rocker/r-ver:4.4.0") |>
  dfi_env(
    DEBIAN_FRONTEND = "noninteractive",
    TZ = "America/Chicago",
    LANG = "en_US.UTF-8"
  )
df

}
\seealso{
\code{\link[=dfi_arg]{dfi_arg()}} for build-time variables &
\href{https://docs.docker.com/engine/reference/builder/#env}{Official Docker \code{ENV} documentation}

Other dockerfile instruction functions: 
\code{\link{dfi_add}()},
\code{\link{dfi_arg}()},
\code{\link{dfi_cmd}()},
\code{\link{dfi_copy}()},
\code{\link{dfi_entrypoint}()},
\code{\link{dfi_expose}()},
\code{\link{dfi_from}()},
\code{\link{dfi_healthcheck}()},
\code{\link{dfi_label}()},
\code{\link{dfi_maintainer}()},
\code{\link{dfi_onbuild}()},
\code{\link{dfi_run}()},
\code{\link{dfi_shell}()},
\code{\link{dfi_stopsignal}()},
\code{\link{dfi_user}()},
\code{\link{dfi_volume}()},
\code{\link{dfi_workdir}()}
}
\concept{dockerfile instruction functions}
