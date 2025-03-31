# Test read_dockerfile() ----
test_that("read_dockerfile(): reads Dockerfile correctly", {
  # Create a temporary Dockerfile for testing
  dockerfile_path <- tempfile(fileext = "")
  writeLines(c(
    "FROM ubuntu:24.10",
    "RUN apt-get update && \\",
    "    apt-get install -y curl",
    "WORKDIR /app",
    "COPY . /app/",
    "CMD [\"bash\"]"
  ), dockerfile_path)
  
  # Read the Dockerfile
  df <- read_dockerfile(dockerfile_path)
  expect_true(is_dockerfile(df))
  expect_equal(length(df$lines), 5) # 5 lines after processing continuations
  expect_equal(df$metadata$base_image, "ubuntu:24.10")
  expect_equal(df$metadata$package_manager, "apt")
  
  # Test error for missing file
  expect_error(read_dockerfile("nonexistent_dockerfile"), "File not found")
  
  # Clean up
  unlink(dockerfile_path)
})

# Test write_dockerfile() ----
test_that("write_dockerfile(): writes Dockerfile correctly", {
  # Create a dockerfile
  df <- dockerfile() |>
    dfi_from("ubuntu:24.10") |>
    dfi_run("apt-get update && apt-get install -y curl") |>
    dfi_workdir("/app") |>
    dfi_cmd("bash")
  
  # Write to temporary file
  dockerfile_path <- tempfile(fileext = "")
  expect_message(
    write_dockerfile(df, dockerfile_path)
  )
  
  # Check that file exists
  expect_true(file.exists(dockerfile_path))
  
  # Read the file back
  lines <- readLines(dockerfile_path)
  expect_true(any(grepl("^FROM ubuntu:24.10", lines)))
  expect_true(any(grepl("^RUN apt-get update", lines)))
  expect_true(any(grepl("^WORKDIR /app", lines)))
  expect_true(any(grepl('^CMD \\[\"bash\"\\]', lines)))
  
  # Test multiline formatting
  multiline_df <- dockerfile() |>
    dfi_from("ubuntu:24.10") |>
    dfi_run("apt-get update && apt-get install -y curl && apt-get clean && rm -rf /var/lib/apt/lists/*")
  
  multiline_path <- tempfile(fileext = "")
  expect_message(
    write_dockerfile(multiline_df, multiline_path, multiline = TRUE)
  )
  
  # Check that the long RUN command was split
  multilines <- readLines(multiline_path)
  expect_true(any(grepl("\\\\$", multilines))) # Check for continuation characters
  
  # Clean up
  unlink(dockerfile_path)
  unlink(multiline_path)
})
