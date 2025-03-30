#' @section Dockerfile Instructions:
#' Functions for adding Dockerfile instructions are prefixed with `dfi_*` and follow the
#' pattern `dfi_instruction(.dockerfile, ...)`.
#'
#' @section Dockerfile Generators:
#' Functions for generating Dockerfiles from various sources are prefixed with `dk_*` and
#' can create Dockerfiles from an R session, renv lockfile, DESCRIPTION file, or R script.
#'
#' @section Dockerfile Modifiers:
#' Functions for modifying Dockerfiles are prefixed with `dfm_*` and allow for adding,
#' removing, replacing, and moving lines within a Dockerfile.
#'
#' @section Dockerignore Management:
#' Functions for managing .dockerignore files are prefixed with `di_*` and allow for
#' adding and removing patterns from a .dockerignore file.
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
