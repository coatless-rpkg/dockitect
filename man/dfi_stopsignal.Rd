% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dockerfile-instructions.R
\name{dfi_stopsignal}
\alias{dfi_stopsignal}
\title{Add a \code{STOPSIGNAL} instruction to a \code{dockerfile}}
\usage{
dfi_stopsignal(dockerfile, signal)
}
\arguments{
\item{dockerfile}{A \code{dockerfile} object}

\item{signal}{Signal for stopping container (e.g., "SIGTERM", "9")}
}
\value{
An updated \code{dockerfile} object with the \code{STOPSIGNAL} instruction added
}
\description{
Adds a \code{STOPSIGNAL} instruction to set the system call signal that will be
sent to the container to exit.
}
\details{
The \code{STOPSIGNAL} instruction sets the system call signal that will be sent to
the container to request it to exit. The signal can be specified as a signal name
in the format \code{SIGNAME} (e.g., \code{SIGTERM}), or as an unsigned number (e.g., \code{15}).

By default, Docker sends \code{SIGTERM} to containers when they need to be stopped.
If the container doesn't exit within the timeout period (default 10 seconds),
Docker sends \code{SIGKILL} to forcibly terminate it.
}
\examples{
# Set SIGTERM as the stop signal
df <- dockerfile() |>
  dfi_from("rocker/r-ver:4.4.0") |>
  dfi_stopsignal("SIGTERM")
df
   
# Set using signal number
df <- dockerfile() |>
  dfi_from("rocker/r-ver:4.4.0") |>
  dfi_stopsignal("15")
df

}
\seealso{
\code{\link[=dfi_healthcheck]{dfi_healthcheck()}} for configuring container health checks &
\href{https://docs.docker.com/engine/reference/builder/#stopsignal}{Official Docker \code{STOPSIGNAL} documentation}

Other dockerfile instruction functions: 
\code{\link{dfi_add}()},
\code{\link{dfi_arg}()},
\code{\link{dfi_cmd}()},
\code{\link{dfi_copy}()},
\code{\link{dfi_entrypoint}()},
\code{\link{dfi_env}()},
\code{\link{dfi_expose}()},
\code{\link{dfi_from}()},
\code{\link{dfi_healthcheck}()},
\code{\link{dfi_label}()},
\code{\link{dfi_maintainer}()},
\code{\link{dfi_onbuild}()},
\code{\link{dfi_run}()},
\code{\link{dfi_shell}()},
\code{\link{dfi_user}()},
\code{\link{dfi_volume}()},
\code{\link{dfi_workdir}()}
}
\concept{dockerfile instruction functions}
