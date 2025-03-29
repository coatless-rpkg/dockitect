# Test dockerfile() ----
test_that("dockerfile(): creates a valid empty dockerfile object", {
  df <- dockerfile()
  expect_true(is_dockerfile(df))
  expect_type(df, "list")
  expect_equal(df$lines, character(0))
  expect_type(df$metadata, "list")
  expect_null(df$metadata$base_image)
  expect_null(df$metadata$package_manager)
  expect_null(df$metadata$r_version)
})

# Test is_dockerfile() ----
test_that("is_dockerfile(): correctly identifies dockerfile objects", {
  df <- dockerfile()
  expect_true(is_dockerfile(df))
  expect_false(is_dockerfile(list()))
  expect_false(is_dockerfile(NULL))
  expect_false(is_dockerfile("string"))
  expect_false(is_dockerfile(dockerignore()))
})

# Test has_instruction() ----
test_that("has_instruction(): checks if an instruction exists in a dockerfile", {
  df <- dockerfile()
  df$lines <- c("FROM ubuntu:24.10", "RUN apt-get update")
  
  expect_true(has_instruction(df, "FROM"))
  expect_true(has_instruction(df, "RUN"))
  expect_true(has_instruction(df, "from")) # Case insensitive
  expect_false(has_instruction(df, "COPY"))
  expect_false(has_instruction(df, "WORKDIR"))
  
  expect_error(has_instruction("not a dockerfile", "FROM"))
})

# Test check_dockerfile() ----
test_that("check_dockerfile(): validates dockerfile objects", {
  df <- dockerfile()
  expect_true(check_dockerfile(df))
  expect_error(check_dockerfile("not a dockerfile"), "Expected a dockerfile object")
  expect_error(check_dockerfile(NULL), "Expected a dockerfile object")
  expect_error(check_dockerfile(dockerignore()), "Expected a dockerfile object")
})

# Test check_dockerignore() ----
test_that("check_dockerignore(): validates dockerignore objects", {
  di <- dockerignore()
  expect_true(check_dockerignore(di))
  expect_error(check_dockerignore("not a dockerignore"), "Expected a dockerignore object")
  expect_error(check_dockerignore(NULL), "Expected a dockerignore object")
  expect_error(check_dockerignore(dockerfile()), "Expected a dockerignore object")
})

# Test print.dockerfile() ----
test_that("print.dockerfile(): prints correctly", {
  df <- dockerfile()
  df$lines <- c("FROM ubuntu:24.10", "RUN apt-get update")
  expect_output(print(df), "FROM ubuntu:24.10\nRUN apt-get update")
})

# Test add_dockerfile_line() (internal function) ----
test_that("add_dockerfile_line(): adds line and updates metadata", {
  df <- dockerfile()
  
  # Test adding FROM
  df <- add_dockerfile_line(df, "FROM", "ubuntu:24.10")
  expect_equal(df$lines, "FROM ubuntu:24.10")
  expect_equal(df$metadata$base_image, "ubuntu:24.10")
  expect_equal(df$metadata$package_manager, "apt")
  
  # Test adding RUN
  df <- add_dockerfile_line(df, "RUN", "apt-get update")
  expect_equal(df$lines, c("FROM ubuntu:24.10", "RUN apt-get update"))
  
  # Test multi-line command
  df <- add_dockerfile_line(df, "RUN", c("apt-get update", "apt-get install -y curl"))
  expect_equal(df$lines, c("FROM ubuntu:24.10", "RUN apt-get update", "RUN apt-get update", "    apt-get install -y curl"))
  
  # Test rocker/r-ver image for R version
  df <- dockerfile()
  df <- add_dockerfile_line(df, "FROM", "rocker/r-ver:4.4.0")
  expect_equal(df$metadata$base_image, "rocker/r-ver:4.4.0")
  expect_equal(df$metadata$package_manager, "apt")
  expect_equal(df$metadata$r_version, "4.4.0")
})
