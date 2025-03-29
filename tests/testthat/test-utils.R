
# Test get_package_manager() ----
test_that("get_package_manager(): detects package manager correctly", {
  expect_equal(get_package_manager("ubuntu:24.10"), "apt")
  expect_equal(get_package_manager("debian:12"), "apt")
  expect_equal(get_package_manager("rocker/r-ver:4.2.0"), "apt")
  expect_equal(get_package_manager("opensuse/leap:15.6"), "zypper")
  expect_equal(get_package_manager("centos:7"), "yum")
  expect_equal(get_package_manager("fedora:35"), "yum")
  expect_equal(get_package_manager("alpine:3.21"), "apk")
  expect_equal(get_package_manager("archlinux:latest"), "pacman")
  
  # Handle registry paths
  expect_equal(get_package_manager("registry.example.com/ubuntu:20.04"), "apt")
  
  # Handle null input
  expect_null(get_package_manager(NULL))
  
  # Default for unknown
  expect_warning(pkg_mgr <- get_package_manager("unknown:latest"), "Could not determine package manager")
  expect_equal(pkg_mgr, "apt")
})
