
# Test determine_package_manager() ----
test_that("determine_package_manager(): detects package manager correctly", {
  expect_equal(determine_package_manager("ubuntu:24.10"), "apt")
  expect_equal(determine_package_manager("debian:12"), "apt")
  expect_equal(determine_package_manager("rocker/r-ver:4.2.0"), "apt")
  expect_equal(determine_package_manager("opensuse/leap:15.6"), "zypper")
  expect_equal(determine_package_manager("centos:7"), "yum")
  expect_equal(determine_package_manager("fedora:35"), "yum")
  expect_equal(determine_package_manager("alpine:3.21"), "apk")
  expect_equal(determine_package_manager("archlinux:latest"), "pacman")
  
  # Handle registry paths
  expect_equal(determine_package_manager("registry.example.com/ubuntu:20.04"), "apt")
  
  # Handle null input
  expect_null(determine_package_manager(NULL))
  
  # Default for unknown
  expect_warning(pkg_mgr <- determine_package_manager("unknown:latest"))
  expect_equal(pkg_mgr, "apt")
})
