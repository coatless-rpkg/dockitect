% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dockitect-templates.R
\name{dk_template_shiny}
\alias{dk_template_shiny}
\title{Create a Shiny app dockerfile template}
\usage{
dk_template_shiny(
  r_version = NULL,
  port = 3838,
  app_dir = ".",
  additional_pkgs = NULL
)
}
\arguments{
\item{r_version}{R version to use (default: current version)}

\item{port}{Port to expose (default: 3838)}

\item{app_dir}{Local directory with Shiny app (default: ".")}

\item{additional_pkgs}{Additional R packages to install}
}
\value{
A dockerfile object
}
\description{
Create a Shiny app dockerfile template
}
