# TODO: add skips if no docker
# LIBS
library(outsider)
library(testthat)

# VARS
repo <- outsider:::vars_get('repo')
pkgnm <- outsider:::vars_get('pkgnm')
fname <- outsider:::vars_get('fname')
img <- outsider:::vars_get('img')

# FUNCTIONS ----
mock_tags <- function(...) {
  readRDS(file = outsider:::datadir_get('tag_data.RData'))
}

# RUNNING
# Bad practice to test the internal functioning, but
# tests are too slow and require running in separate environments otherwise.
context('Testing \'install\'')
test_that("is_installed() works", {
  with_mock(
    `outsider::module_installed` = function(...) data.frame(repo = 'this/repo'),
    expect_true(outsider:::is_installed(repo = 'this/repo'))
  )
  with_mock(
    `outsider::module_installed` = function(...) data.frame(repo = 'this/repo'),
    expect_false(outsider:::is_installed(repo = 'that/repo'))
  )
})
test_that("install() works", {
  with_mock(
    `outsider:::is_installed` = function(...) FALSE,
    `devtools::install_github` = function(...) TRUE,
    `outsider:::docker_pull` = function(...) TRUE,
    `outsider:::docker_build` = function(...) TRUE,
    `outsider::module_uninstall` = function(...) TRUE,
    `outsider:::repo_to_img` = function(...) list(),
    expect_false(outsider:::install(repo = 'this/repo', tag = ''))
  )
  with_mock(
    `outsider:::is_installed` = function(...) TRUE,
    `devtools::install_github` = function(...) TRUE,
    `outsider:::docker_pull` = function(...) TRUE,
    `outsider:::docker_build` = function(...) TRUE,
    `outsider::module_uninstall` = function(...) TRUE,
    `outsider:::repo_to_img` = function(...) list(),
    expect_true(outsider:::install(repo = 'this/repo', tag = '',
                                   dockerfile_url = ''))
  )
  with_mock(
    `outsider:::is_installed` = function(...) TRUE,
    `devtools::install_github` = function(...) TRUE,
    `outsider:::docker_pull` = function(...) TRUE,
    `outsider:::docker_build` = function(...) TRUE,
    `outsider::module_uninstall` = function(...) TRUE,
    `outsider:::repo_to_img` = function(...) list(),
    expect_true(outsider:::install(repo = 'this/repo', tag = ''))
  )
})
