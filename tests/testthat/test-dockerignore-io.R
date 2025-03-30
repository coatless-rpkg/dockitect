# Test read_dockerignore() ----
test_that("read_dockerignore(): reads .dockerignore correctly", {
  # Create a temporary .dockerignore for testing
  dockerignore_path <- tempfile(fileext = "")
  writeLines(c(
    "# Comments should be ignored",
    ".git",
    "*.log",
    "",
    "node_modules/"
  ), dockerignore_path)
  
  # Read the .dockerignore
  di <- read_dockerignore(dockerignore_path)
  expect_true(is_dockerignore(di))
  expect_equal(length(di$patterns), 3) # 3 patterns after filtering
  expect_true(".git" %in% di$patterns)
  expect_true("*.log" %in% di$patterns)
  expect_true("node_modules/" %in% di$patterns)
  
  # Test error for missing file
  expect_error(read_dockerignore("nonexistent_dockerignore"), "File not found")
  
  # Clean up
  unlink(dockerignore_path)
})

# Test write_dockerignore() ----
test_that("write_dockerignore(): writes .dockerignore correctly", {
  # Create a dockerignore object
  di <- dockerignore() |>
    di_add(".git") |>
    di_add("*.log") |>
    di_add("node_modules/")
  
  # Write to temporary file
  dockerignore_path <- tempfile(fileext = "")
  expect_message(
    write_dockerignore(di, dockerignore_path)
  )
  
  # Check that file exists
  expect_true(file.exists(dockerignore_path))
  
  # Read the file back
  lines <- readLines(dockerignore_path)
  expect_equal(length(lines), 3)
  expect_true(".git" %in% lines)
  expect_true("*.log" %in% lines)
  expect_true("node_modules/" %in% lines)
  
  # Clean up
  unlink(dockerignore_path)
})
