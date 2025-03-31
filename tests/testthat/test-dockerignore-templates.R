# Test individual component templates ----
test_that("Component template functions create correct templates", {
  # Test renv template
  di_renv <- dk_template_ignore_renv()
  expect_s3_class(di_renv, "dockerignore")
  expect_true("renv/library/" %in% di_renv$patterns)
  expect_true("renv/staging/" %in% di_renv$patterns)
  
  # Test packrat template
  di_packrat <- dk_template_ignore_packrat()
  expect_s3_class(di_packrat, "dockerignore")
  expect_true("packrat/lib*/" %in% di_packrat$patterns)
  expect_true("packrat/src/" %in% di_packrat$patterns)
  
  # Test raw data template
  di_raw <- dk_template_ignore_raw_data()
  expect_s3_class(di_raw, "dockerignore")
  expect_true("data/raw/" %in% di_raw$patterns)
  expect_true("data/interim/" %in% di_raw$patterns)
})

# Test dk_template_ignore_git() ----
test_that("dk_template_ignore_git(): creates correct template", {
  di <- dk_template_ignore_git()
  
  # Check class
  expect_s3_class(di, "dockerignore")
  
  # Check contents
  expected_patterns <- c(
    ".git/",
    ".gitignore",
    ".gitattributes"
  )
  expect_true(all(expected_patterns %in% di$patterns))
  
  # Check total number of patterns
  expect_gte(length(di$patterns), 4)
})

# Test dk_template_ignore_r() with component options ----
test_that("dk_template_ignore_r(): correctly includes/excludes components", {
  # Test with default options (include all)
  di <- dk_template_ignore_r()
  expect_s3_class(di, "dockerignore")
  
  # Basic R patterns should always be present
  basic_patterns <- c(".Rproj.user/", ".Rhistory", ".RData")
  expect_true(all(basic_patterns %in% di$patterns))
  
  # Default should include both renv and packrat patterns
  expect_true("renv/library/" %in% di$patterns)
  expect_true("packrat/lib*/" %in% di$patterns)
  
  # Test without renv
  di_no_renv <- dk_template_ignore_r(include_renv = FALSE)
  expect_true(all(basic_patterns %in% di_no_renv$patterns))
  expect_false("renv/library/" %in% di_no_renv$patterns)
  expect_true("packrat/lib*/" %in% di_no_renv$patterns)
  
  # Test without packrat
  di_no_packrat <- dk_template_ignore_r(include_packrat = FALSE)
  expect_true(all(basic_patterns %in% di_no_packrat$patterns))
  expect_true("renv/library/" %in% di_no_packrat$patterns)
  expect_false("packrat/lib*/" %in% di_no_packrat$patterns)
  
  # Test with both disabled
  di_basic <- dk_template_ignore_r(include_renv = FALSE, include_packrat = FALSE)
  expect_true(all(basic_patterns %in% di_basic$patterns))
  expect_false("renv/library/" %in% di_basic$patterns)
  expect_false("packrat/lib*/" %in% di_basic$patterns)
})

# Test dk_template_ignore_os() ----
test_that("dk_template_ignore_os(): creates correct template", {
  di <- dk_template_ignore_os()
  expect_s3_class(di, "dockerignore")
  
  expected_patterns <- c(
    ".DS_Store",
    "Thumbs.db",
    "desktop.ini"
  )
  expect_true(all(expected_patterns %in% di$patterns))
  
  # Check total number of patterns
  expect_gte(length(di$patterns), 5)
})

# Test dk_template_ignore_editor() ----
test_that("dk_template_ignore_editor(): creates correct template", {
  di <- dk_template_ignore_editor()
  
  # Check class
  expect_s3_class(di, "dockerignore")
  
  # Check contents for different editors
  vs_code_patterns <- c(".vscode/", "*.code-workspace")
  jetbrains_patterns <- c(".idea/", "*.iml")
  sublime_patterns <- c("*.sublime-workspace", "*.sublime-project")
  vim_patterns <- c("*.swp", "*.swo")
  emacs_patterns <- c("#*#", ".#*")
  
  # Should include patterns for all major editors
  expect_true(all(vs_code_patterns %in% di$patterns))
  expect_true(all(jetbrains_patterns %in% di$patterns))
  expect_true(all(sublime_patterns %in% di$patterns))
  expect_true(all(vim_patterns %in% di$patterns))
  expect_true(all(emacs_patterns %in% di$patterns))
  
  # Check total number of patterns
  expect_gte(length(di$patterns), 15)
})

# Test dk_template_ignore_node() ----
test_that("dk_template_ignore_node(): creates correct template", {
  di <- dk_template_ignore_node()
  expect_s3_class(di, "dockerignore")
  
  expected_patterns <- c(
    "node_modules/",
    "npm-debug.log*",
    "yarn-error.log*",
    "package-lock.json"
  )
  expect_true(all(expected_patterns %in% di$patterns))
  
  # Check total number of patterns
  expect_gte(length(di$patterns), 8)
})

# Test dk_template_ignore_python() ----
test_that("dk_template_ignore_python(): creates correct template", {
  di <- dk_template_ignore_python()
  expect_s3_class(di, "dockerignore")
  
  expected_patterns <- c(
    "__pycache__/",
    "*.py[cod]",
    ".venv/",
    "venv/",
    "*.egg-info/"
  )
  expect_true(all(expected_patterns %in% di$patterns))
  
  # Check total number of patterns
  expect_gte(length(di$patterns), 15)
})

# Test dk_template_ignore_data() with component options ----
test_that("dk_template_ignore_data(): correctly includes/excludes raw data", {
  # Test with default options (include raw)
  di <- dk_template_ignore_data()
  expect_s3_class(di, "dockerignore")
  
  # Data file patterns should always be present
  file_patterns <- c("*.csv", "*.tsv", "*.xls*", "*.sqlite*")
  expect_true(all(file_patterns %in% di$patterns))
  
  # Default should include raw data patterns
  expect_true("data/raw/" %in% di$patterns)
  expect_true("data/processed/" %in% di$patterns)
  
  # Test without raw data
  di_no_raw <- dk_template_ignore_data(include_raw = FALSE)
  expect_true(all(file_patterns %in% di_no_raw$patterns))
  expect_false("data/raw/" %in% di_no_raw$patterns)
  expect_false("data/processed/" %in% di_no_raw$patterns)
})

# Test dk_template_ignore_common() ----
test_that("dk_template_ignore_common(): combines templates correctly", {
  # Test with default settings (git, r, os only)
  di <- dk_template_ignore_common()
  expect_s3_class(di, "dockerignore")
  
  # Should include git patterns
  expect_true(".gitignore" %in% di$patterns)
  # Should include R patterns
  expect_true(".Rhistory" %in% di$patterns)
  # Should include OS patterns
  expect_true(".DS_Store" %in% di$patterns)
  # Should NOT include editor patterns by default
  expect_false(".vscode/" %in% di$patterns)
  # Should NOT include node patterns by default
  expect_false("node_modules/" %in% di$patterns)
  # Should NOT include python patterns by default
  expect_false("*.py[cod]" %in% di$patterns)
  # Should NOT include data patterns by default
  expect_false("*.csv" %in% di$patterns)
  
  # Test with editor enabled
  di_with_editor <- dk_template_ignore_common(editor = TRUE)
  expect_true(".vscode/" %in% di_with_editor$patterns)
  expect_true(".idea/" %in% di_with_editor$patterns)
  
  # Test with everything enabled
  di_all <- dk_template_ignore_common(
    git = TRUE, 
    r = TRUE, 
    os = TRUE,
    editor = TRUE,
    node = TRUE, 
    python = TRUE, 
    data = TRUE
  )
  
  # Should include all requested pattern types
  expect_true(".gitignore" %in% di_all$patterns)
  expect_true(".Rhistory" %in% di_all$patterns)
  expect_true(".DS_Store" %in% di_all$patterns)
  expect_true(".vscode/" %in% di_all$patterns)
  expect_true("node_modules/" %in% di_all$patterns)
  expect_true("*.py[cod]" %in% di_all$patterns)
  expect_true("*.csv" %in% di_all$patterns)
  
  # Test with only editor and python
  di_editor_python <- dk_template_ignore_common(
    git = FALSE, 
    r = FALSE, 
    os = FALSE,
    editor = TRUE, 
    python = TRUE
  )
  
  # Should only include editor and python patterns
  expect_false(".gitignore" %in% di_editor_python$patterns)
  expect_false(".Rhistory" %in% di_editor_python$patterns)
  expect_false(".DS_Store" %in% di_editor_python$patterns)
  expect_true(".vscode/" %in% di_editor_python$patterns)
  expect_true("*.py[cod]" %in% di_editor_python$patterns)
})

# Test parameter passing ----
test_that("dk_template_ignore_common(): passes parameters to sub-templates", {
  # Test passing renv parameter
  di <- dk_template_ignore_common(r = TRUE)
  expect_true(".Rhistory" %in% di$patterns)
  expect_true("renv/library/" %in% di$patterns)
  expect_true("packrat/lib*/" %in% di$patterns)
  
  # Test passing raw data parameter
  di <- dk_template_ignore_common(data = TRUE)
  expect_true("*.csv" %in% di$patterns)
  expect_true("data/raw/" %in% di$patterns)
})

# Test integration with other dockerignore functions ----
test_that("Template functions integrate with di_* functions", {
  # Create a template
  di <- dk_template_ignore_git()
  
  # Add custom pattern
  di <- di_add(di, "my-custom-pattern")
  expect_true("my-custom-pattern" %in% di$patterns)
  
  # Remove a pattern
  di <- di_remove(di, ".gitignore")
  expect_false(".gitignore" %in% di$patterns)
  
  # Replace a pattern
  di <- di_replace(di, ".git/", "git-directory/")
  expect_false(".git/" %in% di$patterns)
  expect_true("git-directory/" %in% di$patterns)
})

# Testing passing an existing dockerignore object
test_that("dk_template_ignore_*(): functions accept existing dockerignore object", {
  # Test passing a dockerignore object to dk_template_ignore_git
  base_di <- dockerignore()
  base_di <- di_add(base_di, "custom-pattern")
  
  # Add git patterns to existing object
  git_di <- dk_template_ignore_git(base_di)
  expect_true(is_dockerignore(git_di))
  expect_true("custom-pattern" %in% git_di$patterns)
  expect_true(".git/" %in% git_di$patterns)
  
  # Test with other template functions
  r_di <- dk_template_ignore_r(base_di)
  expect_true("custom-pattern" %in% r_di$patterns)
  expect_true(".Rhistory" %in% r_di$patterns)
  
  # Test chaining multiple template functions
  chained_di <- base_di |>
    dk_template_ignore_git() |>
    dk_template_ignore_os() |>
    dk_template_ignore_editor()
  
  expect_true("custom-pattern" %in% chained_di$patterns)
  expect_true(".git/" %in% chained_di$patterns)
  expect_true(".DS_Store" %in% chained_di$patterns)
  expect_true(".vscode/" %in% chained_di$patterns)
})

test_that("Individual template functions create correct templates", {
  ## Test dk_template_ignore_git() ----
  di_git <- dk_template_ignore_git()
  expect_s3_class(di_git, "dockerignore")
  expect_true(".git/" %in% di_git$patterns)
  
  ## Test dk_template_ignore_r() ----
  di_r <- dk_template_ignore_r(include_renv = TRUE, include_packrat = FALSE)
  expect_true(".Rhistory" %in% di_r$patterns)
  expect_true("renv/library/" %in% di_r$patterns)
  expect_false("packrat/lib*/" %in% di_r$patterns)
})

# Test dk_template_ignore_common() ----
test_that("dk_template_ignore_common(): works with existing object and passes parameters correctly", {
  base_di <- dockerignore()
  base_di <- di_add(base_di, "custom-pattern")
  
  # Test with selected components
  custom_di <- dk_template_ignore_common(
    .dockerignore = base_di,
    git = TRUE,
    r = FALSE,
    os = FALSE,
    python = TRUE
  )
  
  # Should include base pattern
  expect_true("custom-pattern" %in% custom_di$patterns)
  
  # Should include git patterns
  expect_true(".git/" %in% custom_di$patterns)
  
  # Should include python patterns
  expect_true("*.py[cod]" %in% custom_di$patterns)
  
  # Should NOT include R patterns
  expect_false(".Rhistory" %in% custom_di$patterns)
  
  # Should NOT include OS patterns
  expect_false(".DS_Store" %in% custom_di$patterns)
})