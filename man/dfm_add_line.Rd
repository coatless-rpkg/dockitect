% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dockerfile-modifications.R
\name{dfm_add_line}
\alias{dfm_add_line}
\title{Add a line to a \code{dockerfile} at a specific position}
\usage{
dfm_add_line(dockerfile, line, after = NULL)
}
\arguments{
\item{dockerfile}{A \code{dockerfile} object}

\item{line}{Line to add (raw text)}

\item{after}{Position after which to add the line (default: end of file)}
}
\value{
An updated \code{dockerfile} object with the new line added
}
\description{
Adds a raw line to a `dockerfile`` at a specified position. This is a lower-level
function typically used internally by higher-level functions.
}
\details{
Unlike the instruction-specific functions (\verb{dfi_*}), this function adds
raw text without any formatting or validation. It's useful for adding
comments or custom instructions not covered by the built-in functions.
}
\examples{
df <- dockerfile() |>
  dfi_from("rocker/r-ver:4.4.0")
  
# Add a comment after the FROM instruction
df <- dfm_add_line(df, "# This is a comment", after = 1)

}
\seealso{
\code{\link[=dfm_remove_line]{dfm_remove_line()}} for removing a line &
\code{\link[=dfm_replace_line]{dfm_replace_line()}} for replacing a line

Other dockerfile modification functions: 
\code{\link{dfm_group_similar}()},
\code{\link{dfm_move_line}()},
\code{\link{dfm_remove_line}()},
\code{\link{dfm_replace_line}()},
\code{\link{dfm_sort_by_instruction}()}
}
\concept{dockerfile modification functions}
