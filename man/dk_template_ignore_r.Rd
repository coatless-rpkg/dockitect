% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dockerignore-templates.R
\name{dk_template_ignore_r}
\alias{dk_template_ignore_r}
\title{Create a \code{dockerignore} template for R-related files}
\usage{
dk_template_ignore_r(.dockerignore = NULL, renv = TRUE, packrat = TRUE)
}
\arguments{
\item{.dockerignore}{Optional existing dockerignore object to add patterns to}

\item{renv}{Include renv folders (default: TRUE)}

\item{packrat}{Include packrat folders (default: TRUE)}
}
\value{
A \code{dockerignore} object with R-related ignore patterns
}
\description{
Creates a \code{dockerignore} template with patterns to ignore common R-related files
and directories.
}
\details{
This template adds patterns to ignore common R-related files and directories
that aren't needed in a Docker image, including:
\itemize{
\item \verb{.Rproj.user/}: RStudio user settings
\item \code{.Rhistory}: R command history
\item \code{.RData}: R workspace data
\item \code{.Ruserdata}: R user data
\item \verb{*.Rproj}: RStudio project files
\item \verb{*.rds}: R serialized data files
\item \verb{.Rcheck/}: R package check directories
}

If \code{renv = TRUE}, it also adds patterns to ignore \code{{renv}} library
directories, which should be rebuilt inside the container.

If \code{packrat = TRUE}, it adds patterns to ignore \code{{packrat}} library
directories.
}
\examples{
# Create a new dockerignore with R patterns
di <- dk_template_ignore_r()
di

# Include R patterns but exclude package managers
di <- dk_template_ignore_r(renv = FALSE, packrat = FALSE)
di
}
\seealso{
\code{\link[=dk_template_ignore_renv]{dk_template_ignore_renv()}} for renv-specific patterns,
\code{\link[=dk_template_ignore_packrat]{dk_template_ignore_packrat()}} for packrat-specific patterns, &
\code{\link[=dk_template_ignore_common]{dk_template_ignore_common()}} for a more comprehensive template

Other dockerignore template functions: 
\code{\link{dk_template_ignore_common}()},
\code{\link{dk_template_ignore_data}()},
\code{\link{dk_template_ignore_editor}()},
\code{\link{dk_template_ignore_git}()},
\code{\link{dk_template_ignore_node}()},
\code{\link{dk_template_ignore_os}()},
\code{\link{dk_template_ignore_packrat}()},
\code{\link{dk_template_ignore_python}()},
\code{\link{dk_template_ignore_raw_data}()},
\code{\link{dk_template_ignore_renv}()}
}
\concept{dockerignore template functions}
