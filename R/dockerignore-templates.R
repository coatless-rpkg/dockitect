#' Create a `dockerignore` template for Git-related files
#'
#' Creates a `dockerignore` template with patterns to ignore common Git-related files
#' and directories.
#'
#' @param .dockerignore Optional existing `dockerignore` object to add patterns to
#'
#' @return
#' A `dockerignore` object with Git-related ignore patterns
#'
#' @examples
#' # Create a new dockerignore with Git patterns
#' di <- dk_template_ignore_git()
#' di
#' 
#' # Add Git patterns to an existing dockerignore
#' di <- dockerignore() |>
#'   di_add("*.log") |>
#'   dk_template_ignore_git()
#' di
#' 
#' @details
#' This template adds patterns to ignore common Git-related files and directories,
#' including:
#' 
#' - `.git/`: The Git repository directory
#' - `.gitignore`: Git ignore files
#' - `.gitattributes`: Git attributes file
#' - `.github/`: GitHub-specific files
#' - `.gitlab-ci.yml`: GitLab CI configuration
#'
#' These files are typically not needed in a Docker image and can reduce the
#' build context size.
#'
#' @seealso
#' [dk_template_ignore_common()] for a more comprehensive template &
#' [di_add()] for adding custom patterns
#'
#' @family dockerignore template functions
#' @export
dk_template_ignore_git <- function(.dockerignore = NULL) {
  # Create or use existing dockerignore object
  di <- if (!is.null(.dockerignore)) {
    check_dockerignore(.dockerignore)
    .dockerignore
  } else {
    dockerignore()
  }
  
  patterns <- c(
    ".git/",
    ".gitignore",
    ".gitattributes",
    ".github/",
    ".gitlab-ci.yml"
  )
  di_add(di, patterns)
}

#' Create a dockerignore template for `{renv}`-related files
#'
#' Creates a `dockerignore` template with patterns to ignore `{renv}` library and cache
#' directories.
#'
#' @param .dockerignore Optional existing `dockerignore` object to add patterns to
#'
#' @return
#' A `dockerignore` object with `{renv}`-related ignore patterns
#'
#' @examples
#' # Create a new dockerignore with renv patterns
#' di <- dk_template_ignore_renv()
#' di
#' 
#' # Add renv patterns to an existing dockerignore
#' di <- dockerignore() |>
#'   di_add("*.log") |>
#'   dk_template_ignore_renv()
#' di
#'
#' @details
#' This template adds patterns to ignore `{renv}` library and cache directories,
#' which are typically not needed in a Docker image. These include:
#' 
#' - `renv/library/`: Package library
#' - `renv/staging/`: Staging area for packages
#' - `renv/python/`: Python libraries
#' - `renv/local/`: Local cache
#' 
#' When using `{renv}` in a Dockerfile, you typically want to copy just the
#' `renv.lock` file and use `renv::restore()` to rebuild the library inside
#' the container, rather than copying the entire library.
#'
#' @seealso
#' [dk_template_ignore_r()] for R-specific patterns,
#' [dk_from_renv()] for creating a Dockerfile from an renv.lock file, &
#' [dk_template_ignore_common()] for a more comprehensive template
#'
#' @family dockerignore template functions
#' @export
dk_template_ignore_renv <- function(.dockerignore = NULL) {
  # Create or use existing dockerignore object
  di <- if (!is.null(.dockerignore)) {
    check_dockerignore(.dockerignore)
    .dockerignore
  } else {
    dockerignore()
  }
  
  patterns <- c(
    "renv/library/",
    "renv/staging/",
    "renv/python/",
    "renv/local/"
  )
  di_add(di, patterns)
}

#' Create a `dockerignore` template for `{packrat}`-related files
#'
#' Creates a `dockerignore` template with patterns to ignore `{packrat}` library and
#' source directories.
#'
#' @param .dockerignore Optional existing `dockerignore` object to add patterns to
#'
#' @return
#' A `dockerignore` object with `{packrat}`-related ignore patterns
#'
#' @examples
#' # Create a new dockerignore with packrat patterns
#' di <- dk_template_ignore_packrat()
#' 
#' # Add packrat patterns to an existing dockerignore
#' di <- dockerignore() |>
#'   di_add("*.log") |>
#'   dk_template_ignore_packrat()
#'
#' @details
#' This template adds patterns to ignore `{packrat}` library and source directories,
#' which are typically not needed in a Docker image. These include:
#' 
#' - `packrat/lib*/`: Package libraries
#' - `packrat/src/`: Package sources
#' 
#' When using `{packrat}` in a **Dockerfile**, you typically want to copy just the
#' `{packrat}` configuration files and use `{packrat}`'s restore functionality to
#' rebuild the library inside the container, rather than copying the entire
#' library.
#'
#' @seealso
#' [dk_template_ignore_renv()] for renv-specific patterns,
#' [dk_template_ignore_r()] for R-specific patterns, &
#' [dk_template_ignore_common()] for a more comprehensive template
#'
#' @family dockerignore template functions
#' @export
dk_template_ignore_packrat <- function(.dockerignore = NULL) {
  # Create or use existing dockerignore object
  di <- if (!is.null(.dockerignore)) {
    check_dockerignore(.dockerignore)
    .dockerignore
  } else {
    dockerignore()
  }
  
  patterns <- c(
    "packrat/lib*/",
    "packrat/src/"
  )
  di_add(di, patterns)
}

#' Create a `dockerignore` template for R-related files
#'
#' Creates a `dockerignore` template with patterns to ignore common R-related files
#' and directories.
#'
#' @param .dockerignore Optional existing dockerignore object to add patterns to
#' @param renv          Include renv folders (default: TRUE)
#' @param packrat       Include packrat folders (default: TRUE)
#'
#' @return
#' A `dockerignore` object with R-related ignore patterns
#'
#' @examples
#' # Create a new dockerignore with R patterns
#' di <- dk_template_ignore_r()
#' di
#' 
#' # Include R patterns but exclude package managers
#' di <- dk_template_ignore_r(renv = FALSE, packrat = FALSE)
#' di
#' @details
#' This template adds patterns to ignore common R-related files and directories
#' that aren't needed in a Docker image, including:
#' 
#' - `.Rproj.user/`: RStudio user settings
#' - `.Rhistory`: R command history
#' - `.RData`: R workspace data
#' - `.Ruserdata`: R user data
#' - `*.Rproj`: RStudio project files
#' - `*.rds`: R serialized data files
#' - `.Rcheck/`: R package check directories
#' 
#' If `renv = TRUE`, it also adds patterns to ignore `{renv}` library
#' directories, which should be rebuilt inside the container.
#' 
#' If `packrat = TRUE`, it adds patterns to ignore `{packrat}` library
#' directories.
#'
#' @seealso
#' [dk_template_ignore_renv()] for renv-specific patterns,
#' [dk_template_ignore_packrat()] for packrat-specific patterns, &
#' [dk_template_ignore_common()] for a more comprehensive template
#'
#' @family dockerignore template functions
#' @export
dk_template_ignore_r <- function(.dockerignore = NULL, renv = TRUE, packrat = TRUE) {
  # Create or use existing dockerignore object
  di <- if (!is.null(.dockerignore)) {
    check_dockerignore(.dockerignore)
    .dockerignore
  } else {
    dockerignore()
  }
  
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
  
  if (renv) {
    di <- dk_template_ignore_renv(di)
  }
  
  if (packrat) {
    di <- dk_template_ignore_packrat(di)
  }
  
  di
}

#' Create a `dockerignore` template for OS-related files
#'
#' Creates a `dockerignore` template with patterns to ignore operating system
#' specific files and directories.
#'
#' @param .dockerignore Optional existing `dockerignore` object to add patterns to
#'
#' @return
#' A `dockerignore` object with OS-related ignore patterns
#'
#' @examples
#' # Create a new dockerignore with OS patterns
#' di <- dk_template_ignore_os()
#' di
#' 
#' # Add OS patterns to an existing dockerignore
#' di <- dockerignore() |>
#'   di_add("*.log") |>
#'   dk_template_ignore_os()
#' di
#' 
#' @details
#' This template adds patterns to ignore operating system specific files
#' that aren't needed in a Docker image, including:
#' 
#' - `.DS_Store`: macOS folder metadata
#' - `Thumbs.db`: Windows thumbnail cache
#' - `desktop.ini`: Windows folder configuration
#' - `*.swp`, `*~`: Temporary editor files
#' - `.directory`: KDE folder metadata
#' - `Icon?`: macOS icon files
#' - `*.lnk`: Windows shortcuts
#'
#' These files are typically not needed in a Docker image and can reduce the
#' build context size.
#'
#' @seealso
#' [dk_template_ignore_editor()] for editor-specific patterns &
#' [dk_template_ignore_common()] for a more comprehensive template
#'
#' @family dockerignore template functions
#' @export
dk_template_ignore_os <- function(.dockerignore = NULL) {
  # Create or use existing dockerignore object
  di <- if (!is.null(.dockerignore)) {
    check_dockerignore(.dockerignore)
    .dockerignore
  } else {
    dockerignore()
  }
  
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
#' Creates a `dockerignore` template with patterns to ignore files and directories
#' created by common code editors and IDEs.
#'
#' @param .dockerignore Optional existing `dockerignore` object to add patterns to
#'
#' @return
#' A `dockerignore` object with editor-related ignore patterns
#'
#' @examples
#' # Create a new dockerignore with editor patterns
#' di <- dk_template_ignore_editor()
#' di
#' 
#' # Add editor patterns to an existing dockerignore
#' di <- dockerignore() |>
#'   di_add("*.log") |>
#'   dk_template_ignore_editor()
#' di
#' 
#' @details
#' This template adds patterns to ignore files and directories created by
#' common code editors and IDEs, including:
#' 
#' - VS Code: `.vscode/`, `*.code-workspace`
#' - JetBrains IDEs (e.g., RStudio, PyCharm): `.idea/`, `*.iml`, `*.iws`
#' - Sublime Text: `*.sublime-workspace`, `*.sublime-project`
#' - Vim: `*.swp`, `*.swo`, `*~`
#' - Emacs: `#*#`, `.#*`, `.projectile`
#' - Eclipse: `.classpath`, `.project`, `.settings/`
#' - Visual Studio: `.vs/`, `*.suo`, `*.njsproj`, `*.sln`
#'
#' These files are typically not needed in a Docker image and can reduce the
#' build context size.
#'
#' @seealso
#' [dk_template_ignore_os()] for OS-specific patterns &
#' [dk_template_ignore_common()] for a more comprehensive template
#'
#' @family dockerignore template functions
#' @export
dk_template_ignore_editor <- function(.dockerignore = NULL) {
  # Create or use existing dockerignore object
  di <- if (!is.null(.dockerignore)) {
    check_dockerignore(.dockerignore)
    .dockerignore
  } else {
    dockerignore()
  }
  
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

#' Create a `dockerignore` template for Node.js/JavaScript projects
#'
#' Creates a `dockerignore` template with patterns to ignore Node.js and
#' JavaScript related files and directories.
#'`
#' @param .dockerignore Optional existing `dockerignore` object to add patterns to
#'
#' @return
#' A `dockerignore` object with Node.js-related ignore patterns
#'
#' @examples
#' # Create a new dockerignore with Node.js patterns
#' di <- dk_template_ignore_node()
#' di 
#' 
#' # Add Node.js patterns to an existing dockerignore
#' di <- dockerignore() |>
#'   di_add("*.log") |>
#'   dk_template_ignore_node()
#' di
#' 
#' @details
#' This template adds patterns to ignore Node.js and JavaScript related files
#' and directories, including:
#' 
#' - `node_modules/`: Package dependencies
#' - `npm-debug.log*`, `yarn-error.log*`: Package manager logs
#' - `package-lock.json`, `yarn.lock`: Lock files
#' - `.npm/`, `.yarn/`: Cache directories
#' - `*.tgz`: Packaged modules
#' - `.pnp.*`: Plug'n'Play files
#' - `.cache/`: Build cache
#' - `.eslintcache`: ESLint cache
#'
#' These files are typically not needed in a Docker image or can be regenerated
#' during the build process. Ignoring them can significantly reduce the build
#' context size, especially `node_modules/` which can be very large.
#'
#' @seealso
#' [dk_template_ignore_python()] for Python-specific patterns &
#' [dk_template_ignore_common()] for a more comprehensive template
#'
#' @family dockerignore template functions
#' @export
dk_template_ignore_node <- function(.dockerignore = NULL) {
  # Create or use existing dockerignore object
  di <- if (!is.null(.dockerignore)) {
    check_dockerignore(.dockerignore)
    .dockerignore
  } else {
    dockerignore()
  }
  
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

#' Create a `dockerignore` template for Python projects
#'
#' Creates a `dockerignore` template with patterns to ignore Python related files
#' and directories.
#'
#' @param .dockerignore Optional existing `dockerignore` object to add patterns to
#'
#' @return
#' A `dockerignore` object with Python-related ignore patterns
#'
#' @examples
#' # Create a new dockerignore with Python patterns
#' di <- dk_template_ignore_python()
#' 
#' # Add Python patterns to an existing dockerignore
#' di <- dockerignore() |>
#'   di_add("*.log") |>
#'   dk_template_ignore_python()
#'
#' @details
#' This template adds patterns to ignore Python related files and directories,
#' including:
#' 
#' - `__pycache__/`: Compiled Python files
#' - `*.py[cod]`: Python compiled files
#' - `.pytest_cache/`: PyTest cache
#' - `.coverage`, `htmlcov/`: Coverage reports
#' - `.tox/`, `.nox/`: Testing environments
#' - `.venv/`, `venv/`, `ENV/`: Virtual environments
#' - `.Python`, `build/`, `dist/`: Build artifacts
#' - `.egg-info/`, `*.egg`: Package metadata
#' - `.ipynb_checkpoints/`: Jupyter notebook checkpoints
#'
#' These files are typically not needed in a Docker image or can be regenerated
#' during the build process. Ignoring them can significantly reduce the build
#' context size.
#'
#' @seealso
#' [dk_template_ignore_node()] for Node.js-specific patterns &
#' [dk_template_ignore_common()] for a more comprehensive template
#'
#' @family dockerignore template functions
#' @export
dk_template_ignore_python <- function(.dockerignore = NULL) {
  # Create or use existing dockerignore object
  di <- if (!is.null(.dockerignore)) {
    check_dockerignore(.dockerignore)
    .dockerignore
  } else {
    dockerignore()
  }
  
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

#' Create a `dockerignore` template for raw data directories
#'
#' Creates a `dockerignore` template with patterns to ignore common raw data
#' directories.
#'
#' @param .dockerignore Optional existing dockerignore object to add patterns to
#'
#' @return
#' A `dockerignore` object with raw data directory patterns
#'
#' @examples
#' # Create a new dockerignore with raw data patterns
#' di <- dk_template_ignore_raw_data()
#' 
#' # Add raw data patterns to an existing dockerignore
#' di <- dockerignore() |>
#'   di_add("*.log") |>
#'   dk_template_ignore_raw_data()
#'
#' @details
#' This template adds patterns to ignore common raw data directories following
#' the [`usethis`](https://usethis.r-lib.org/reference/use_data.html) 
#' and [Cookiecutter Data Science](https://cookiecutter-data-science.drivendata.org/#directory-structure) conventions:
#' 
#' - `data-raw/`
#' - `data/raw/`: Raw, immutable data
#' - `data/interim/`: Intermediate data that has been transformed
#' - `data/processed/`: The final, canonical data sets for modeling
#' - `data/external/`: Data from third party sources
#'
#' Raw data is often large and not needed in the Docker image. Instead, it's
#' usually better to mount the data as a volume at runtime.
#'
#' @seealso
#' [dk_template_ignore_data()] for ignoring data file formats,
#' [dk_template_ignore_common()] for a more comprehensive template,
#' [Cookiecutter Data Science Directory Structure Overview](https://cookiecutter-data-science.drivendata.org/#directory-structure), &
#' [`usethis::use_raw_data()`](https://usethis.r-lib.org/reference/use_data.html)
#'
#' @family dockerignore template functions
#' @export
dk_template_ignore_raw_data <- function(.dockerignore = NULL) {
  # Create or use existing dockerignore object
  di <- if (!is.null(.dockerignore)) {
    check_dockerignore(.dockerignore)
    .dockerignore
  } else {
    dockerignore()
  }
  
  patterns <- c(
    "data-raw/",
    "data/raw/",
    "data/interim/",
    "data/processed/",
    "data/external/"
  )
  di_add(di, patterns)
}

#' Create a `dockerignore` template for data-related files
#'
#' Creates a `dockerignore`` template with patterns to ignore common data files
#' and directories.
#'
#' @param .dockerignore Optional existing dockerignore object to add patterns to
#' @param raw           Include raw data directories (default: TRUE)
#'
#' @return
#' A `dockerignore` object with data-related ignore patterns
#'
#' @examples
#' # Create a new dockerignore with data patterns
#' di <- dk_template_ignore_data()
#' 
#' # Exclude raw data directories
#' di <- dk_template_ignore_data(raw = FALSE)
#'
#' @details
#' This template adds patterns to ignore common data files and directories that
#' are often large and can significantly slow down Docker builds. It includes
#' patterns for various data file formats:
#' 
#' - `data/`: Data directories
#' - `*.csv`, `*.tsv`: CSV and TSV files
#' - `*.xls*`: Excel files
#' - `*.db`, `*.sqlite*`: Database files
#' - `*.h5`, `*.hdf5`: HDF5 files
#' - `*.parquet`, `*.feather`: Columnar storage formats
#' - `*.json`: JSON files
#' - `*.pickle`: Python pickle files
#' - `*.rdata`, `*.rda`: R data files
#' 
#' If `raw = TRUE`, it also ignores common data directories:
#' 
#' - `data-raw/`, `data/raw/`: Raw data
#' - `data/interim/`: Intermediate processed data
#' - `data/processed/`: Final processed data
#' - `data/external/`: Data from external sources
#'
#' @seealso
#' [dk_template_ignore_raw_data()] for raw data directories only &
#' [dk_template_ignore_common()] for a more comprehensive template
#'
#' @family dockerignore template functions
#' @export
dk_template_ignore_data <- function(.dockerignore = NULL, raw = TRUE) {
  # Create or use existing dockerignore object
  di <- if (!is.null(.dockerignore)) {
    check_dockerignore(.dockerignore)
    .dockerignore
  } else {
    dockerignore()
  }
  
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
  
  if (raw) {
    di <- dk_template_ignore_raw_data(di)
  }
  
  di
}

#' Create a `dockerignore` template for common ignore patterns
#'
#' Creates a comprehensive **.dockerignore** template with patterns for various
#' file categories based on the selected options.
#'
#' @param .dockerignore Optional existing dockerignore object to add patterns to
#' @param git           Include Git-related files (default: TRUE)
#' @param r             Include R-related files (default: TRUE)
#' @param os            Include OS-specific files (default: TRUE)
#' @param editor        Include editor-specific files (default: FALSE)
#' @param node          Include Node.js-related files (default: FALSE)
#' @param python        Include Python-related files (default: FALSE)
#' @param data          Include data-related files (default: FALSE)
#'
#' @return
#' A `dockerignore` object with selected ignore patterns
#'
#' @examples
#' # Basic template with Git, R, and OS patterns
#' di <- dk_template_ignore_common()
#' 
#' # Customize with additional categories
#' di <- dk_template_ignore_common(
#'   git = TRUE,
#'   r = TRUE,
#'   os = TRUE,
#'   editor = TRUE,
#'   data = TRUE
#' )
#'
#' @details
#' This function provides a convenient way to create a comprehensive .dockerignore
#' file tailored to your project's needs. Each category adds patterns relevant to
#' specific types of files:
#' 
#' - `git`: Git repositories, .gitignore, etc.
#' - `r`: R history, RData, Rproj.user, etc.
#' - `os`: .DS_Store, Thumbs.db, etc.
#' - `editor`: .vscode, .idea, *.swp, etc.
#' - `node`: node_modules, package-lock.json, etc.
#' - `python`: __pycache__, *.pyc, venv, etc.
#' - `data`: *.csv, *.json, *.xlsx, etc.
#'
#' @seealso
#' [dk_template_ignore_git()] for Git-specific patterns,
#' [dk_template_ignore_r()] for R-specific patterns, &
#' [di_add()] for adding custom patterns
#'
#' @family dockerignore template functions
#' @export
dk_template_ignore_common <- function(
    .dockerignore = NULL, git = TRUE, r = TRUE, os = TRUE, 
    editor = FALSE, node = FALSE, python = FALSE, 
    data = FALSE) {
  # Create or use existing dockerignore object
  result <- if (!is.null(.dockerignore)) {
    check_dockerignore(.dockerignore)
    .dockerignore
  } else {
    dockerignore()
  }
  
  # Add patterns for selected categories
  if (git) {
    result <- dk_template_ignore_git(result)
  }
  
  if (r) {
    result <- dk_template_ignore_r(result)
  }
  
  if (os) {
    result <- dk_template_ignore_os(result)
  }
  
  if (editor) {
    result <- dk_template_ignore_editor(result)
  }
  
  if (node) {
    result <- dk_template_ignore_node(result)
  }
  
  if (python) {
    result <- dk_template_ignore_python(result)
  }
  
  if (data) {
    result <- dk_template_ignore_data(result)
  }
  
  result
}