# Test dockerignore() ----
test_that("dockerignore(): creates a valid empty dockerignore object", {
  di <- dockerignore()
  expect_true(is_dockerignore(di))
  expect_type(di, "list")
  expect_equal(di$patterns, character(0))
})

# Test is_dockerignore() ----
test_that("is_dockerignore(): correctly identifies dockerignore objects", {
  di <- dockerignore()
  expect_true(is_dockerignore(di))
  expect_false(is_dockerignore(list()))
  expect_false(is_dockerignore(NULL))
  expect_false(is_dockerignore("string"))
  expect_false(is_dockerignore(dockerfile()))
})

# Test check_dockerignore() ----
test_that("check_dockerignore(): validates dockerignore objects", {
  di <- dockerignore()
  expect_true(check_dockerignore(di))
  expect_error(check_dockerignore("not a dockerignore"), "Expected a dockerignore object")
  expect_error(check_dockerignore(NULL), "Expected a dockerignore object")
  expect_error(check_dockerignore(dockerfile()), "Expected a dockerignore object")
})

# Test print.dockerignore ----
test_that("print.dockerignore(): prints correctly", {
  di <- dockerignore()
  di$patterns <- c(".git", "*.Rdata")
  expect_output(print(di), ".git\\n\\*.Rdata")
})

# Test c.dockerignore() ----
test_that("c.dockerignore(): combines dockerignore objects correctly", {
  di1 <- dockerignore()
  di1 <- di_add(di1, c("pattern1", "pattern2"))
  
  di2 <- dockerignore()
  di2 <- di_add(di2, c("pattern3", "pattern4"))
  
  # Combine with c()
  combined <- c(di1, di2)
  expect_s3_class(combined, "dockerignore")
  
  # Check that all patterns are present
  expected_patterns <- c("pattern1", "pattern2", "pattern3", "pattern4")
  expect_true(all(expected_patterns %in% combined$patterns))
  
  # Combine more than two objects
  di3 <- dockerignore()
  di3 <- di_add(di3, "pattern5")
  
  combined2 <- c(di1, di2, di3)
  expect_true(all(c(expected_patterns, "pattern5") %in% combined2$patterns))
  
  # Combine with an empty dockerignore
  empty <- dockerignore()
  combined3 <- c(empty, di1)
  expect_equal(sort(combined3$patterns), sort(di1$patterns))
  
  # Test error for non-dockerignore object
  expect_error(c(di1, "not-a-dockerignore"), "must be dockerignore objects")
})

