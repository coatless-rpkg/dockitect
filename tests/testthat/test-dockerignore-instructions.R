# Test di_add() ----
test_that("di_add(): adds patterns correctly", {
  di <- dockerignore()
  
  # Test single pattern
  di <- di_add(di, "node_modules")
  expect_equal(di$patterns, "node_modules")
  
  # Test adding multiple patterns at once
  di <- dockerignore()
  di <- di_add(di, c("node_modules", "*.log", "tmp/"))
  expect_equal(di$patterns, c("node_modules", "*.log", "tmp/"))
  expect_equal(length(di$patterns), 3)
  
  # Test handling of duplicates
  di <- di_add(di, c("node_modules", "build/"))
  expect_equal(di$patterns, c("node_modules", "*.log", "tmp/", "build/"))
  expect_equal(length(di$patterns), 4)
  
  # Adding the same pattern again shouldn't duplicate it
  di <- di_add(di, "node_modules")
  expect_equal(length(di$patterns), 4)
  expect_equal(sum(di$patterns == "node_modules"), 1)
})

# Test di_remove() ----
test_that("di_remove(): removes patterns correctly", {
  di <- dockerignore()
  di <- di_add(di, c("node_modules", "*.log", "tmp/", "build/", ".env"))
  
  # Test single pattern
  di <- di_remove(di, "*.log")
  expect_false("*.log" %in% di$patterns)
  expect_equal(length(di$patterns), 4)
  
  # Test removing multiple patterns at once
  di <- di_remove(di, c("node_modules", "build/"))
  expect_false(any(c("node_modules", "build/") %in% di$patterns))
  expect_equal(di$patterns, c("tmp/", ".env"))
  
  # Test removing non-existent pattern (should not error)
  di_before <- di
  di <- di_remove(di, "non-existent-pattern")
  expect_equal(di, di_before)
  
  # Test removing multiple with some non-existent
  di <- di_remove(di, c("tmp/", "another-non-existent"))
  expect_equal(di$patterns, ".env")
})

# Test di_replace() ----
test_that("di_replace(): replaces patterns correctly", {
  di <- dockerignore()
  di <- di_add(di, c("logs/", "node_modules/", "tmp/", ".env", "*.cache"))
  
  # Test single pattern replacement
  di <- di_replace(di, "logs/", "log-files/")
  expect_true("log-files/" %in% di$patterns)
  expect_false("logs/" %in% di$patterns)
  
  # Test replacing multiple patterns with a single new pattern
  di <- di_replace(di, c("node_modules/", "tmp/"), "dependencies/")
  expect_false(any(c("node_modules/", "tmp/") %in% di$patterns))
  expect_equal(sum(di$patterns == "dependencies/"), 2)
  
  # Test replacing with 1:1 mapping (equal length vectors)
  di <- dockerignore()
  di <- di_add(di, c("logs/", "node_modules/", "tmp/"))
  di <- di_replace(di, 
                   c("logs/", "node_modules/", "tmp/"), 
                   c("log-files/", "dependencies/", "temporary/"))
  expect_equal(di$patterns, c("log-files/", "dependencies/", "temporary/"))
  
  # Test error on unequal lengths (when new_pattern > 1)
  di <- dockerignore()
  di <- di_add(di, c("logs/", "node_modules/"))
  expect_error(di_replace(di, 
                          c("logs/", "node_modules/"), 
                          c("log-files/", "dependencies/", "extra/")))
  
  # Test handling of non-existent pattern
  di <- dockerignore()
  di <- di_add(di, "logs/")
  di_before <- di
  di <- di_replace(di, "non-existent", "replacement")
  expect_equal(di, di_before)
})
