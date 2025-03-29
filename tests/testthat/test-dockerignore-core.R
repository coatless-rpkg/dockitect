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
