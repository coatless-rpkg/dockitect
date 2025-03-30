# Test dk_register_template() ----
test_that("dk_register_template(): registers and retrieves custom templates", {
  # Define a test template
  test_template <- function(name = "test") {
    dockerfile() |>
      dfi_from("ubuntu:24.10") |>
      dfi_run(paste("echo", name))
  }
  
  # Register the template
  dk_register_template("test_template", test_template)
  
  # Use the template
  df <- dk_template_custom("test_template", name = "hello")
  expect_true(is_dockerfile(df))
  expect_equal(df$lines[1], "FROM ubuntu:24.10")
  expect_equal(df$lines[2], "RUN echo hello")
  
  # Test error for non-existent template
  expect_error(dk_template_custom("nonexistent_template"), "Template .* not found")
  
  # Test error for non-function template
  expect_error(dk_register_template("bad_template", "not a function"), "must be a function")
})

# Test dk_template_base() ----
test_that("dk_template_base(): creates a base R dockerfile", {
  
  df <- dk_template_base(r_version = "4.4.0")
  expect_true(is_dockerfile(df))
  expect_true(has_instruction(df, "FROM"))
  expect_true(has_instruction(df, "LABEL"))
  expect_true(has_instruction(df, "WORKDIR"))
  expect_true(has_instruction(df, "RUN"))
  expect_true(has_instruction(df, "VOLUME"))
  expect_true(has_instruction(df, "CMD"))
  
  # Check if R version is correct
  expect_equal(df$metadata$base_image, "rocker/r-ver:4.4.0")
  
  # Test with additional packages
  df <- dk_template_base(r_version = "4.4.0", additional_pkgs = c("dplyr", "ggplot2"))
  expect_true(any(grepl("install.packages", df$lines)))
  expect_true(any(grepl("dplyr", df$lines)))
  expect_true(any(grepl("ggplot2", df$lines)))
})

# Test dk_template_shiny() ----
test_that("dk_template_shiny(): creates a Shiny app dockerfile", {

  df <- dk_template_shiny(r_version = "4.4.0", port = 3838)
  expect_true(is_dockerfile(df))
  expect_true(has_instruction(df, "FROM"))
  expect_true(has_instruction(df, "LABEL"))
  expect_true(has_instruction(df, "COPY"))
  expect_true(has_instruction(df, "WORKDIR"))
  expect_true(has_instruction(df, "EXPOSE"))
  expect_true(has_instruction(df, "CMD"))
  
  # Check if base image is correct
  expect_equal(df$metadata$base_image, "rocker/shiny:4.4.0")
  
  # Check if port is exposed
  expect_true(any(grepl("EXPOSE 3838", df$lines)))
  
  # Test with additional packages
  df <- dk_template_shiny(r_version = "4.4.0", additional_pkgs = c("dplyr", "ggplot2"))
  expect_true(any(grepl("install.packages", df$lines)))
  expect_true(any(grepl("dplyr", df$lines)))
  expect_true(any(grepl("ggplot2", df$lines)))
})

# Test dk_template_plumber() ----
test_that("dk_template_plumber(): creates a Plumber API dockerfile", {

  df <- dk_template_plumber(r_version = "4.4.0", port = 8000, api_file = "plumber.R")
  expect_true(is_dockerfile(df))
  expect_true(has_instruction(df, "FROM"))
  expect_true(has_instruction(df, "LABEL"))
  expect_true(has_instruction(df, "COPY"))
  expect_true(has_instruction(df, "WORKDIR"))
  expect_true(has_instruction(df, "EXPOSE"))
  expect_true(has_instruction(df, "CMD"))
  
  # Check if base image is correct
  expect_equal(df$metadata$base_image, "rocker/r-ver:4.4.0")
  
  # Check if port is exposed
  expect_true(any(grepl("EXPOSE 8000", df$lines)))
  
  # Check if plumber is installed
  expect_true(any(grepl("plumber", df$lines)))
  
  # Test with additional packages
  df <- dk_template_plumber(r_version = "4.4.0", additional_pkgs = c("dplyr", "jsonlite"))
  expect_true(any(grepl("install.packages", df$lines)))
  expect_true(any(grepl("dplyr", df$lines)))
  expect_true(any(grepl("jsonlite", df$lines)))
})
