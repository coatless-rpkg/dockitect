
#' Create a dockerignore template for Git-related files
#'
#' @return A dockerignore object with Git-related ignore patterns
#' @export
dk_template_ignore_git <- function() {
  di <- dockerignore()
  patterns <- c(
    ".git/",
    ".gitignore",
    ".gitattributes",
    ".github/",
    ".gitlab-ci.yml"
  )
  di_add(di, patterns)
}

#' Create a dockerignore template for renv-related files
#'
#' @return A dockerignore object with renv-related ignore patterns
#' @export
dk_template_ignore_renv <- function() {
  di <- dockerignore()
  patterns <- c(
    "renv/library/",
    "renv/staging/",
    "renv/python/",
    "renv/local/"
  )
  di_add(di, patterns)
}

#' Create a dockerignore template for packrat-related files
#'
#' @return A dockerignore object with packrat-related ignore patterns
#' @export
dk_template_ignore_packrat <- function() {
  di <- dockerignore()
  patterns <- c(
    "packrat/lib*/",
    "packrat/src/"
  )
  di_add(di, patterns)
}

#' Create a dockerignore template for R-related files
#'
#' @param include_renv Include renv folders (default: TRUE)
#' @param include_packrat Include packrat folders (default: TRUE)
#' @return A dockerignore object with R-related ignore patterns
#' @export
dk_template_ignore_r <- function(include_renv = TRUE, include_packrat = TRUE) {
  di <- dockerignore()
  patterns <- c(
    ".Rproj.user/",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "*.Rproj",
    "*.rds",
    ".lintr",
    "*.Rcheck/",
    ".Renviron",
    ".Rprofile"
  )
  di <- di_add(di, patterns)
  
  if (include_renv) {
    di <- c(di, dk_template_ignore_renv())
  }
  
  if (include_packrat) {
    di <- c(di, dk_template_ignore_packrat())
  }
  
  di
}

#' Create a dockerignore template for OS-related files
#'
#' @return A dockerignore object with OS-related ignore patterns
#' @export
dk_template_ignore_os <- function() {
  di <- dockerignore()
  patterns <- c(
    ".DS_Store",
    "Thumbs.db",
    "desktop.ini",
    "*.swp",
    "*~",
    ".directory",
    "Icon?",
    "ehthumbs.db",
    "*.lnk"
  )
  di_add(di, patterns)
}

#' Create a dockerignore template for editor-related files
#'
#' @return A dockerignore object with editor-related ignore patterns
#' @export
dk_template_ignore_editor <- function() {
  di <- dockerignore()
  patterns <- c(
    # VS Code
    ".vscode/",
    "*.code-workspace",
    # JetBrains IDEs
    ".idea/",
    "*.iml",
    "*.iws",
    "*.ipr",
    "out/",
    # Sublime Text
    "*.sublime-workspace",
    "*.sublime-project",
    # Vim
    "*.swp",
    "*.swo",
    "*~",
    # Emacs
    "#*#",
    ".#*",
    ".projectile",
    # Eclipse
    ".classpath",
    ".project",
    ".settings/",
    # Visual Studio
    ".vs/",
    "*.suo",
    "*.ntvs*",
    "*.njsproj",
    "*.sln",
    "*.sw?"
  )
  di_add(di, patterns)
}

#' Create a dockerignore template for Node.js/JavaScript projects
#'
#' @return A dockerignore object with Node.js-related ignore patterns
#' @export
dk_template_ignore_node <- function() {
  di <- dockerignore()
  patterns <- c(
    "node_modules/",
    "npm-debug.log*",
    "yarn-error.log*",
    "yarn-debug.log*",
    "package-lock.json",
    "yarn.lock",
    ".npm/",
    ".yarn/",
    "*.tgz",
    ".pnp.*",
    ".cache/",
    ".eslintcache",
    ".node_repl_history"
  )
  di_add(di, patterns)
}

#' Create a dockerignore template for Python projects
#'
#' @return A dockerignore object with Python-related ignore patterns
#' @export
dk_template_ignore_python <- function() {
  di <- dockerignore()
  patterns <- c(
    "__pycache__/",
    "*.py[cod]",
    "*$py.class",
    ".pytest_cache/",
    ".coverage",
    "htmlcov/",
    ".tox/",
    ".nox/",
    ".venv/",
    "venv/",
    "ENV/",
    "env/",
    "*.so",
    ".Python",
    "build/",
    "develop-eggs/",
    "dist/",
    "downloads/",
    "eggs/",
    ".eggs/",
    "lib/",
    "lib64/",
    "parts/",
    "sdist/",
    "var/",
    "wheels/",
    "*.egg-info/",
    "*.egg",
    ".ipynb_checkpoints/",
    "*.pyc",
    "pip-log.txt",
    "pip-delete-this-directory.txt"
  )
  di_add(di, patterns)
}

#' Create a dockerignore template for raw data directories
#'
#' @return A dockerignore object with raw data directory patterns
#' @export
dk_template_ignore_raw_data <- function() {
  di <- dockerignore()
  patterns <- c(
    "data/raw/",
    "data/interim/",
    "data/processed/",
    "data/external/"
  )
  di_add(di, patterns)
}

#' Create a dockerignore template for common data files
#'
#' @param include_raw Include raw data directories (default: TRUE)
#' @return A dockerignore object with data-related ignore patterns
#' @export
dk_template_ignore_data <- function(include_raw = TRUE) {
  di <- dockerignore()
  patterns <- c(
    "*.csv",
    "*.tsv",
    "*.xls*",
    "*.db",
    "*.sqlite*",
    "*.h5",
    "*.hdf5",
    "*.parquet",
    "*.feather",
    "*.json",
    "*.pickle",
    "*.sav",
    "*.rdata",
    "*.rda"
  )
  di <- di_add(di, patterns)
  
  if (include_raw) {
    di <- c(di, dk_template_ignore_raw_data())
  }
  
  di
}

#' Create a dockerignore template combining multiple categories
#'
#' @param git Include Git-related files (default: TRUE)
#' @param r Include R-related files (default: TRUE)
#' @param os Include OS-specific files (default: TRUE)
#' @param editor Include editor-specific files (default: FALSE)
#' @param node Include Node.js-related files (default: FALSE)
#' @param python Include Python-related files (default: FALSE)
#' @param data Include data-related files (default: FALSE)
#' @return A dockerignore object with combined ignore patterns
#' @export
dk_template_ignore_common <- function(git = TRUE, r = TRUE, os = TRUE, 
                                      editor = FALSE, node = FALSE, python = FALSE, 
                                      data = FALSE) {
  result <- dockerignore()
  
  if (git) {
    result <- c(result, dk_template_ignore_git())
  }
  
  if (r) {
    result <- c(result, dk_template_ignore_r())
  }
  
  if (os) {
    result <- c(result, dk_template_ignore_os())
  }
  
  if (editor) {
    result <- c(result, dk_template_ignore_editor())
  }
  
  if (node) {
    result <- c(result, dk_template_ignore_node())
  }
  
  if (python) {
    result <- c(result, dk_template_ignore_python())
  }
  
  if (data) {
    result <- c(result, dk_template_ignore_data())
  }
  
  result
}
