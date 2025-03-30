# c.f. https://github.com/r-lib/pkgcache/tree/main?tab=readme-ov-file#using-pkgcache-in-cran-packages

# Temporary directory for R package cache
# Workaround until going full toward renv
Sys.setenv(R_USER_CACHE_DIR = tempfile())
