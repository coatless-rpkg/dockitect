
# Test dk_from_session() ----
test_that("dk_from_session(): creates dockerfile from current session", {
  # Skip if 'pak' not available
  skip_if_not_installed("pak")
  
  # Test with default parameters
  df <- dk_from_session(include_packages = FALSE)
  expect_true(is_dockerfile(df))
  expect_true(has_instruction(df, "FROM"))
  
  # Check if R version is included in base image
  expect_match(df$metadata$base_image, paste0("rocker/r-ver:", paste(R.version$major, R.version$minor, sep = ".")))
  
  # Test with custom base image
  df <- dk_from_session(base_image = "ubuntu:24.10", include_packages = FALSE)
  expect_equal(df$metadata$base_image, "ubuntu:24.10")
})

# Test dk_add_sysreqs() ----
test_that("dk_add_sysreqs(): adds system requirements correctly", {
  # Skip if 'pak' not available
  skip_if_not_installed("pak")
  
  # Mock functions for testing to avoid long loading times ---- 
  mock_pkg_sysreqs <- function(packages, sysreqs_platform = "ubuntu") {
    # Create the packages data frame with tibble-like structure
    packages_df <- structure(
      list(
        sysreq = c("fontconfig", "freetype", "fribidi", "git", "gnumake", "harfbuzz", 
                   "libcurl", "libgit2", "libicu", "libjpeg", "libpng", "libtiff", 
                   "libxml2", "openssl", "pandoc", "x11", "zlib"),
        
        # Each element is a character vector of R packages that need this system requirement
        packages = list(
          "systemfonts",
          c("ragg", "systemfonts", "textshaping"),
          "textshaping",
          c("credentials", "gitcreds", "remotes"),
          c("fs", "httpuv", "sass"),
          "textshaping",
          "curl",
          "gert",
          "stringi",
          "ragg",
          "ragg",
          "ragg",
          "xml2",
          c("curl", "openssl"),
          c("knitr", "pkgdown", "rmarkdown"),
          "clipr",
          "httpuv"
        ),
        
        # Pre-installation commands (empty for all entries in this example)
        pre_install = replicate(17, character(0), simplify = FALSE),
        
        # The actual system packages to install
        system_packages = list(
          "libfontconfig1-dev",
          "libfreetype6-dev",
          "libfribidi-dev",
          "git",
          "make",
          "libharfbuzz-dev",
          "libcurl4-openssl-dev",
          "libgit2-dev",
          "libicu-dev",
          "libjpeg-dev",
          "libpng-dev",
          "libtiff-dev",
          "libxml2-dev",
          "libssl-dev",
          "pandoc",
          "libx11-dev",
          "zlib1g-dev"
        ),
        
        # Post-installation commands (empty for all entries in this example)
        post_install = replicate(17, character(0), simplify = FALSE)
      ),
      class = c("tbl", "data.frame"),
      row.names = c(NA, -17L)
    )
    
    # Create the full pak_sysreqs object
    structure(
      list(
        os = "linux",
        distribution = "debian",
        version = NA_character_,
        pre_install = "apt-get -y update",
        install_scripts = paste0("apt-get -y install libx11-dev git libcurl4-openssl-dev ",
                                 "libssl-dev make libgit2-dev zlib1g-dev pandoc libfreetype6-dev ",
                                 "libjpeg-dev libpng-dev libtiff-dev libicu-dev libfontconfig1-dev ",
                                 "libfribidi-dev libharfbuzz-dev libxml2-dev"),
        post_install = character(0),
        packages = packages_df
      ),
      class = "pak_sysreqs"
    )
  }
  
  
  # Create basic dockerfile for apt
  df_apt_base <- dockerfile() |>
    dfi_from("ubuntu:24.10")
  
  # Test with apt package manager
  testthat::with_mocked_bindings(
    {
      df_apt <- dk_add_sysreqs(df_apt_base, c("httr", "xml2"), "apt")
      expect_true(has_instruction(df_apt, "RUN"))
      expect_true(any(grepl("apt-get", df_apt$lines)))
      expect_true(any(grepl("apt-get install", df_apt$lines)))
      expect_true(any(grepl("libcurl4-openssl-dev", df_apt$lines)))
    },
    pkg_sysreqs = mock_pkg_sysreqs,
    .package = "pak"
  )
  
  # Create basic dockerfile for yum
  df_yum_base <- dockerfile() |>
    dfi_from("centos:7")
  
  # Test with yum package manager
  testthat::with_mocked_bindings(
    {
      df_yum <- dk_add_sysreqs(df_yum_base, c("httr", "xml2"), "yum")
      expect_true(has_instruction(df_yum, "RUN"))
      expect_true(any(grepl("yum", df_yum$lines)))
      expect_true(any(grepl("yum install", df_yum$lines)))
      expect_true(any(grepl("libcurl4-openssl-dev", df_yum$lines)))
    },
    pkg_sysreqs = mock_pkg_sysreqs,
    .package = "pak"
  )
  
  # Create basic dockerfile for apk
  df_apk_base <- dockerfile() |>
    dfi_from("alpine:3.21")
  
  df_apk <- dk_add_sysreqs(df_apk_base, c("httr", "xml2"), "apk")
  expect_true(has_instruction(df_apk, "RUN"))
  expect_true(any(grepl("apk", df_apk$lines)))
  expect_true(any(grepl("apk add", df_apk$lines)))
  expect_true(any(grepl("libcurl4-openssl-dev", df_apk$lines)))
  
  
})

# Test dk_from_renv() with a temporary renv.lock file ----
test_that("dk_from_renv(): creates dockerfile from renv.lock", {
  # Create a temporary directory
  temp_dir <- tempdir()
  original_dir <- getwd()
  
  # Create a minimal renv.lock file in the temporary directory
  renv_lock_content <- '{
    "R": {
      "Version": "4.4.0"
    },
    "Packages": {
      "dplyr": {
        "Package": "dplyr",
        "Version": "1.1.4",
        "Source": "Repository",
        "Repository": "CRAN"
      },
      "ggplot2": {
        "Package": "ggplot2",
        "Version": "3.5.1",
        "Source": "Repository",
        "Repository": "CRAN"
      }
    }
  }'
  
  lock_file_path <- file.path(temp_dir, "renv.lock")
  writeLines(renv_lock_content, lock_file_path)
  
  # Run the function with our temporary file
  df <- dk_from_renv(lock_file = lock_file_path, include_sysreqs = FALSE)
  
  # Check the result
  expect_true(is_dockerfile(df))
  expect_true(has_instruction(df, "FROM"))
  expect_true(has_instruction(df, "WORKDIR"))
  expect_true(has_instruction(df, "COPY"))
  expect_true(has_instruction(df, "RUN"))
  
  # Verify R version was extracted
  expect_equal(df$metadata$r_version, "4.4.0")
  
  # Check that the dockerfile includes renv-related commands
  expect_true(any(grepl("renv::init", df$lines)))
  expect_true(any(grepl("renv::restore", df$lines)))
  
  # Clean up
  unlink(lock_file_path)
})


# Test dk_from_description() with a temporary DESCRIPTION file ----
test_that("dk_from_description(): creates dockerfile from DESCRIPTION", {
  # Create a temporary directory
  temp_dir <- tempdir()
  
  # Create a minimal DESCRIPTION file in the temporary directory
  desc_content <- c(
    "Package: testpkg",
    "Title: Test Package",
    "Version: 0.1.0",
    "Authors@R: person('Test', 'User', email = 'test@example.com', role = c('aut', 'cre'))",
    "Description: A test package.",
    "Depends: R (>= 4.4.0)",
    "Imports: dplyr, tidyr",
    "Suggests: testthat, knitr",
    "License: MIT + file LICENSE"
  )
  
  desc_file_path <- file.path(temp_dir, "DESCRIPTION")
  writeLines(desc_content, desc_file_path)
  
  # Run the function with our temporary file
  df <- dk_from_description(description_file = desc_file_path, include_sysreqs = FALSE)
  
  # Check the result
  expect_true(is_dockerfile(df))
  expect_true(has_instruction(df, "FROM"))
  expect_true(has_instruction(df, "WORKDIR"))
  expect_true(has_instruction(df, "COPY"))
  expect_true(has_instruction(df, "RUN"))
  
  # Verify R version was extracted
  expect_equal(df$metadata$r_version, "4.4.0")
  
  # Check that the dockerfile includes package installation commands
  expect_true(any(grepl("install.packages", df$lines)))
  expect_true(any(grepl("dplyr", df$lines)))
  expect_true(any(grepl("tidyr", df$lines)))
  
  # Clean up
  unlink(desc_file_path)
})

# Test dk_from_script ----
test_that("dk_from_script(): creates a dockerfile from an R script", {
  # Create a temporary R script file for testing
  temp_script <- tempfile(fileext = ".R")
  script_content <- '
  library(dplyr)
  require(ggplot2)
  
  # Some sample code
  mtcars %>%
    filter(cyl == 6) %>%
    ggplot(aes(x = wt, y = mpg)) +
    geom_point()
  '
  writeLines(script_content, temp_script)
  
  # Test with temporary script file
  df <- dk_from_script(temp_script, include_sysreqs = FALSE)
  
  expect_true(is_dockerfile(df))
  expect_true(has_instruction(df, "FROM"))
  expect_true(has_instruction(df, "LABEL"))
  expect_true(has_instruction(df, "WORKDIR"))
  expect_true(has_instruction(df, "COPY"))
  expect_true(has_instruction(df, "CMD"))
  expect_true(any(grepl("install.packages", df$lines)))
  expect_true(any(grepl("dplyr|ggplot2", df$lines)))

  # Clean up
  unlink(temp_script)
})
