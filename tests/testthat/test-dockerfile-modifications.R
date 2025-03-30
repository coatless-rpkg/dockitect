# Test dfm_add_line() ----
test_that("dfm_add_line(): adds line at specified position", {
  df <- dockerfile()
  df$lines <- c("FROM ubuntu:20.04", "RUN apt-get update")
  
  # Add at the end (default)
  df <- dfm_add_line(df, "CMD bash")
  expect_equal(df$lines, c("FROM ubuntu:20.04", "RUN apt-get update", "CMD bash"))
  
  # Add after specific position
  df <- dfm_add_line(df, "WORKDIR /app", after = 1)
  expect_equal(df$lines, c("FROM ubuntu:20.04", "WORKDIR /app", "RUN apt-get update", "CMD bash"))
  
  # Add at position out of range (should add at end)
  df <- dfm_add_line(df, "USER nobody", after = 10)
  expect_equal(df$lines, c("FROM ubuntu:20.04", "WORKDIR /app", "RUN apt-get update", "CMD bash", "USER nobody"))
})

# Test dfm_remove_line() ----
test_that("dfm_remove_line(): removes line at specified position", {
  df <- dockerfile()
  df$lines <- c("FROM ubuntu:20.04", "WORKDIR /app", "RUN apt-get update", "CMD bash")
  
  # Remove line
  df <- dfm_remove_line(df, 2)
  expect_equal(df$lines, c("FROM ubuntu:20.04", "RUN apt-get update", "CMD bash"))
  
  # Try to remove line out of range (should warn and return unchanged)
  expect_warning(df2 <- dfm_remove_line(df, 10), "out of range")
  expect_equal(df, df2)
})

# Test dfm_replace_line() ----
test_that("dfm_replace_line(): replaces line at specified position", {
  df <- dockerfile()
  df$lines <- c("FROM ubuntu:20.04", "WORKDIR /app", "RUN apt-get update")
  
  # Replace line
  df <- dfm_replace_line(df, 2, "WORKDIR /home/user")
  expect_equal(df$lines, c("FROM ubuntu:20.04", "WORKDIR /home/user", "RUN apt-get update"))
  
  # Try to replace line out of range (should warn and return unchanged)
  expect_warning(df2 <- dfm_replace_line(df, 10, "CMD bash"), "out of range")
  expect_equal(df, df2)
})

# Test dfm_move_line() ----
test_that("dfm_move_line(): moves line from one position to another", {
  df <- dockerfile()
  df$lines <- c("FROM ubuntu:20.04", "WORKDIR /app", "RUN apt-get update", "CMD bash")
  
  # Move line up
  df <- dfm_move_line(df, 3, 2)
  expect_equal(df$lines, c("FROM ubuntu:20.04", "RUN apt-get update", "WORKDIR /app", "CMD bash"))
  
  # Move line down
  df <- dfm_move_line(df, 1, 3)
  expect_equal(df$lines, c("RUN apt-get update", "WORKDIR /app", "FROM ubuntu:20.04", "CMD bash"))
  
  # Try to move line with out of range source (should warn and return unchanged)
  expect_warning(df2 <- dfm_move_line(df, 10, 1), "Source line .* is out of range")
  expect_equal(df, df2)
  
  # Try to move line to out of range target (should warn and return unchanged)
  expect_warning(df2 <- dfm_move_line(df, 1, 10), "Target position .* is out of range")
  expect_equal(df, df2)
})

# Test dfm_group_similar() ----
test_that("dfm_group_similar(): groups similar instructions", {
  df <- dockerfile()
  df$lines <- c(
    "FROM ubuntu:20.04",
    "RUN apt-get update",
    "RUN apt-get install -y curl",
    "WORKDIR /app",
    "COPY file1.txt /app/",
    "COPY file2.txt /app/"
  )
  
  # Group similar instructions
  grouped <- dfm_group_similar(df)
  
  # FROM and WORKDIR should remain separate
  expect_true(any(grepl("^FROM ", grouped$lines)))
  expect_true(any(grepl("^WORKDIR ", grouped$lines)))
  
  # RUN commands should be combined with &&
  expect_true(any(grepl("^RUN apt-get update.*&&.*apt-get install", grouped$lines)))
  
  # COPY commands should remain separate
  expect_equal(sum(grepl("^COPY ", grouped$lines)), 2)
  
  # Test with continuation lines
  df <- dockerfile()
  df$lines <- c(
    "FROM ubuntu:20.04",
    "RUN apt-get update \\",
    "    && apt-get install -y curl",
    "RUN echo 'done'"
  )
  
  grouped <- dfm_group_similar(df)
  # The continuation line should be preserved
  expect_true(any(grepl("apt-get update", grouped$lines)))
  expect_true(any(grepl("echo 'done'", grouped$lines)))
})

# Test dfm_sort_by_instruction() ----
test_that("dfm_sort_by_instruction(): sorts instructions by type", {
  df <- dockerfile()
  df$lines <- c(
    "LABEL maintainer=user@example.com",
    "RUN apt-get update",
    "FROM ubuntu:20.04",
    "WORKDIR /app",
    "ENV PATH=/usr/local/bin",
    "CMD bash"
  )
  
  # Sort with default order
  sorted <- dfm_sort_by_instruction(df)
  
  # Check the order of instructions
  from_idx <- grep("^FROM ", sorted$lines)
  label_idx <- grep("^LABEL ", sorted$lines)
  env_idx <- grep("^ENV ", sorted$lines)
  workdir_idx <- grep("^WORKDIR ", sorted$lines)
  run_idx <- grep("^RUN ", sorted$lines)
  cmd_idx <- grep("^CMD ", sorted$lines)
  
  expect_true(from_idx < label_idx)
  expect_true(label_idx < env_idx)
  expect_true(env_idx < workdir_idx)
  expect_true(workdir_idx < run_idx)
  expect_true(run_idx < cmd_idx)
  
  # Test with custom order
  custom_sorted <- dfm_sort_by_instruction(df, order = c("FROM", "WORKDIR", "RUN", "ENV", "LABEL", "CMD"))
  
  # Check the custom order
  from_idx <- grep("^FROM ", custom_sorted$lines)
  workdir_idx <- grep("^WORKDIR ", custom_sorted$lines)
  run_idx <- grep("^RUN ", custom_sorted$lines)
  env_idx <- grep("^ENV ", custom_sorted$lines)
  label_idx <- grep("^LABEL ", custom_sorted$lines)
  cmd_idx <- grep("^CMD ", custom_sorted$lines)
  
  expect_true(from_idx < workdir_idx)
  expect_true(workdir_idx < run_idx)
  expect_true(run_idx < env_idx)
  expect_true(env_idx < label_idx)
  expect_true(label_idx < cmd_idx)
})
