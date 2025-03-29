# Test dfi_from() ----
test_that("dfi_from(): adds FROM instruction correctly", {
  df <- dockerfile()
  
  # Basic usage
  df <- dfi_from(df, "ubuntu:24.10")
  expect_equal(df$lines[1], "FROM ubuntu:24.10")
  
  # With AS option
  df <- dockerfile()
  df <- dfi_from(df, "ubuntu:24.10", as = "build")
  expect_equal(df$lines[1], "FROM ubuntu:24.10 AS build")
  
  # Updates metadata
  expect_equal(df$metadata$base_image, "ubuntu:24.10")
  expect_equal(df$metadata$package_manager, "apt")
})

# Test dfi_run() ----
test_that("dfi_run(): adds RUN instruction correctly", {
  df <- dockerfile()
  
  # Single command
  df <- dfi_run(df, "apt-get update")
  expect_equal(df$lines[1], "RUN apt-get update")
  
  # Multiple commands
  df <- dfi_run(df, c("apt-get update", "apt-get install -y curl"))
  expect_equal(df$lines[2], "RUN apt-get update")
  expect_equal(df$lines[3], "    apt-get install -y curl")
})

# Test dfi_copy() ----
test_that("dfi_copy(): adds COPY instruction correctly", {
  df <- dockerfile()
  
  # Basic usage
  df <- dfi_copy(df, "src", "dest")
  expect_equal(df$lines[1], "COPY src dest")
  
  # With from option
  df <- dfi_copy(df, "src", "dest", from = "build")
  expect_equal(df$lines[2], "COPY --from=build src dest")
})

# Test dfi_workdir() ----
test_that("dfi_workdir(): adds WORKDIR instruction correctly", {
  df <- dockerfile()
  df <- dfi_workdir(df, "/app")
  expect_equal(df$lines[1], "WORKDIR /app")
})

# Test dfi_cmd() ----
test_that("dfi_cmd(): adds CMD instruction correctly", {
  df <- dockerfile()
  
  # Single command format
  df <- dfi_cmd(df, "echo hello")
  expect_equal(df$lines[1], "CMD echo hello")
  
  # Array format
  df <- dfi_cmd(df, c("echo", "hello"))
  expect_match(df$lines[2], '^CMD \\["echo","hello"\\]$')
})

# Test dfi_env() ----
test_that("dfi_env(): adds ENV instructions correctly", {
  df <- dockerfile()
  
  # Single env var
  df <- dfi_env(df, "PATH" = "/usr/local/bin")
  expect_equal(df$lines[1], "ENV PATH /usr/local/bin")
  
  # Multiple env vars
  df <- dfi_env(df, HOME = "/home/user", DEBUG = "true")
  expect_equal(df$lines[2], "ENV HOME /home/user")
  expect_equal(df$lines[3], "ENV DEBUG true")
  
  # Empty call should return unchanged
  df_before <- df
  df <- dfi_env(df)
  expect_equal(df, df_before)
})

# Test dfi_expose() ----
test_that("dfi_expose(): adds EXPOSE instruction correctly", {
  df <- dockerfile()
  
  # Single port
  df <- dfi_expose(df, 8080)
  expect_equal(df$lines[1], "EXPOSE 8080")
  
  # Multiple ports
  df <- dfi_expose(df, c(80, 443))
  expect_equal(df$lines[2], "EXPOSE 80 443")
})

# Test dfi_label() ----
test_that("dfi_label(): adds LABEL instruction correctly", {
  df <- dockerfile()
  
  # Single label
  df <- dfi_label(df, maintainer = "user@example.com")
  expect_equal(df$lines[1], 'LABEL maintainer="user@example.com"')
  
  # Multiple labels
  df <- dfi_label(df, version = "1.0", description = "My image")
  expect_equal(df$lines[2], 'LABEL version="1.0" description="My image"')
  
  # Handle quotes in values
  df <- dfi_label(df, quote = 'Contains "quotes"')
  expect_equal(df$lines[3], 'LABEL quote=\"Contains \"quotes\"\"')
  
  # Empty call should return unchanged
  df_before <- df
  df <- dfi_label(df)
  expect_equal(df, df_before)
})

# Test dfi_add() ----
test_that("dfi_add(): adds ADD instruction correctly", {
  df <- dockerfile()
  df <- dfi_add(df, "http://example.com/file.tar.gz", "/tmp/")
  expect_equal(df$lines[1], "ADD http://example.com/file.tar.gz /tmp/")
})

# Test dfi_entrypoint() ----
test_that("dfi_entrypoint(): adds ENTRYPOINT instruction correctly", {
  df <- dockerfile()
  
  # Single command format
  df <- dfi_entrypoint(df, "/entrypoint.sh")
  expect_equal(df$lines[1], "ENTRYPOINT /entrypoint.sh")
  
  # Array format
  df <- dfi_entrypoint(df, c("/bin/bash", "-c"))
  expect_match(df$lines[2], '^ENTRYPOINT \\["/bin/bash","-c"\\]$')
})

# Test dfi_user() ----
test_that("dfi_user(): adds USER instruction correctly", {
  df <- dockerfile()
  
  # Username only
  df <- dfi_user(df, "nobody")
  expect_equal(df$lines[1], "USER nobody")
  
  # Username and group
  df <- dfi_user(df, "www-data", "www-data")
  expect_equal(df$lines[2], "USER www-data:www-data")
  
  # Numeric UID
  df <- dfi_user(df, 1000)
  expect_equal(df$lines[3], "USER 1000")
})

# Test dfi_volume() ----
test_that("dfi_volume(): adds VOLUME instruction correctly", {
  df <- dockerfile()
  
  # Single volume
  df <- dfi_volume(df, "/data")
  expect_equal(df$lines[1], "VOLUME /data")
  
  # Multiple volumes
  df <- dfi_volume(df, c("/data", "/logs"))
  expect_match(df$lines[2], '^VOLUME \\["/data","/logs"\\]$')
})

# Test dfi_arg() ----
test_that("dfi_arg(): adds ARG instruction correctly", {
  df <- dockerfile()
  
  # Name only
  df <- dfi_arg(df, "VERSION")
  expect_equal(df$lines[1], "ARG VERSION")
  
  # Name with default value
  df <- dfi_arg(df, "DEBUG", "false")
  expect_equal(df$lines[2], "ARG DEBUG=false")
})

# Test dfi_healthcheck() ----
test_that("dfi_healthcheck(): adds HEALTHCHECK instruction correctly", {
  df <- dockerfile()
  
  # Basic command
  df <- dfi_healthcheck(df, "curl -f http://localhost/ || exit 1")
  expect_equal(df$lines[1], "HEALTHCHECK CMD curl -f http://localhost/ || exit 1")
  
  # With options
  df <- dfi_healthcheck(df, 
                        "curl -f http://localhost/ || exit 1",
                        interval = "30s",
                        timeout = "5s",
                        start_period = "5s",
                        retries = 3)
  expect_equal(df$lines[2], "HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 CMD curl -f http://localhost/ || exit 1")
})

# Test dfi_shell() ----
test_that("dfi_shell(): adds SHELL instruction correctly", {
  df <- dockerfile()
  
  # Array format directly
  df <- dfi_shell(df, '["/bin/bash", "-c"]')
  expect_equal(df$lines[1], 'SHELL ["/bin/bash", "-c"]')
  
  # Array created from vector
  df <- dfi_shell(df, c("/bin/sh", "-c"))
  expect_match(df$lines[2], '^SHELL \\["/bin/sh","-c"\\]$')
  
  # String parsed to array
  df <- dfi_shell(df, "/bin/bash -c")
  expect_match(df$lines[3], '^SHELL \\["[^"]+","[^"]+"\\]$')
})

# Test dfi_stopsignal() ----
test_that("dfi_stopsignal(): adds STOPSIGNAL instruction correctly", {
  df <- dockerfile()
  df <- dfi_stopsignal(df, "SIGKILL")
  expect_equal(df$lines[1], "STOPSIGNAL SIGKILL")
})

# Test dfi_maintainer() ----
test_that("dfi_maintainer(): adds MAINTAINER instruction with warning", {
  df <- dockerfile()
  expect_warning(df <- dfi_maintainer(df, "user@example.com"), "MAINTAINER is deprecated")
  expect_equal(df$lines[1], "MAINTAINER user@example.com")
})

# Test dfi_onbuild() ----
test_that("dfi_onbuild(): adds ONBUILD instruction correctly", {
  df <- dockerfile()
  df <- dfi_onbuild(df, "RUN npm install")
  expect_equal(df$lines[1], "ONBUILD RUN npm install")
})
