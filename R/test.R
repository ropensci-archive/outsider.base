# Module test functions

# Internal ----
# Ensure tests do not run on Travis-CI
is_running_on_travis <- function() {
  Sys.getenv("CI") == "true" && Sys.getenv("TRAVIS") == "true"
}
