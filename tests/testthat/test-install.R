# TODO: add skips if no docker
context('Testing \'install\'')
test_that("is_installed() works", {
  expect_false(outsider.base:::is_installed(pkgnm = 'notapkg'))
})
test_that("install() and uninstall() works", {
  expect_true(dir.exists(file.path(mdl_flpth, 'inst')))
  expect_true(file.exists(file.path(mdl_flpth, 'inst', 'om.yml')))
  res <- install(flpth = mdl_flpth, tag = 'latest', pull = FALSE)
  pkgs <- utils::installed.packages()
  pkgnms <- unname(pkgs[ ,'Package'])
  expect_true(pkgnm %in% pkgnms)
  expect_true(file.exists(system.file('om.yml', package = pkgnm)))
  expect_true(pkgnm %in% modules_list())
  expect_equal(res, 0L)
  res <- uninstall(pkgnm = pkgnm)
  expect_true(res)
})
