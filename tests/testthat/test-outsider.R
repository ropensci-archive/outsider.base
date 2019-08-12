context('Testing \'outsider\'')
test_that("outsider class and methods works", {
  skip_if(!docker_available)
  container <- structure(list(), class = 'container')
  # test init
  otsdr <- with_mock(
    `outsider.base:::container_init` = function(...) container,
    outsider_init(pkgnm = 'pkgnm')
  )
  expect_true(inherits(otsdr, 'outsider'))
  # test print
  with_mock(
    `outsider.base:::status.container` = function(...) 'This is a mock',
    expect_null(print(otsdr))
  )
  # test run
  otsdr$cmd <- 'cmd'
  with_mock(
    `outsider.base:::start.container` = function(x, ...) TRUE,
    `outsider.base:::halt.container` = function(x, ...) TRUE,
    `outsider.base:::copy.container` = function(x, ...) TRUE,
    `outsider.base:::run.container` = function(x, ...) TRUE,
    expect_true(run(otsdr))
  )
  otsdr$files_to_send <- 'file'
  with_mock(
    `outsider.base:::start.container` = function(x, ...) TRUE,
    `outsider.base:::halt.container` = function(x, ...) TRUE,
    `outsider.base:::copy.container` = function(x, ...) TRUE,
    `outsider.base:::run.container` = function(x, ...) TRUE,
    expect_true(run(otsdr))
  )
  otsdr$wd <- 'wd'
  with_mock(
    `outsider.base:::start.container` = function(x, ...) TRUE,
    `outsider.base:::halt.container` = function(x, ...) TRUE,
    `outsider.base:::copy.container` = function(x, ...) TRUE,
    `outsider.base:::run.container` = function(x, ...) TRUE,
    expect_true(run(otsdr))
  )
  with_mock(
    `outsider.base:::start.container` = function(x, ...) TRUE,
    `outsider.base:::halt.container` = function(x, ...) TRUE,
    `outsider.base:::copy.container` = function(x, ...) TRUE,
    `outsider.base:::run.container` = function(x, ...) FALSE,
    expect_false(run(otsdr))
  )
  with_mock(
    `outsider.base:::start.container` = function(x, ...) FALSE,
    `outsider.base:::halt.container` = function(x, ...) TRUE,
    `outsider.base:::copy.container` = function(x, ...) TRUE,
    `outsider.base:::run.container` = function(x, ...) TRUE,
    expect_false(run(otsdr))
  )
  with_mock(
    `outsider.base:::start.container` = function(x, ...) TRUE,
    `outsider.base:::halt.container` = function(x, ...) TRUE,
    `outsider.base:::copy.container` = function(x, ...) TRUE,
    `outsider.base:::exec.container` = function(x, ...) stop(),
    `outsider.base:::status.container` = function(...) 'This is a mock',
    expect_error(outsider.base:::run.outsider(otsdr))
  )
})
