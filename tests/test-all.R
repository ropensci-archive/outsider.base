library(testthat)

datadir_get <- function(subdir = "") {
  wd <- getwd()
  if (grepl("testthat", wd)) {
    datadir <- "data"
  } else {
    datadir <- file.path("tests", "testthat", "data")
  }
  file.path(datadir, subdir)
}

test_check("outsider")
