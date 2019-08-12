context('Testing \'docker\'')
test_that('is_docker_available() works', {
  with_mock(
    `outsider.base:::is_docker_installed` = function(...) FALSE,
    `outsider.base:::is_docker_running` = function(...) FALSE,
    expect_error(outsider.base:::is_docker_available())
  )
  with_mock(
    `outsider.base:::is_docker_installed` = function(...) TRUE,
    `outsider.base:::is_docker_running` = function(...) FALSE,
    expect_error(outsider.base:::is_docker_available())
  )
  with_mock(
    `outsider.base:::is_docker_installed` = function(...) TRUE,
    `outsider.base:::is_docker_running` = function(...) TRUE,
    expect_true(outsider.base:::is_docker_available())
  )
})
test_that('is_docker_installed() works', {
  with_mock(
    `sys::exec_internal` = function(...) list('status' = 1),
    expect_false(outsider.base:::is_docker_installed())
  )
  skip_if(!docker_available)
  expect_true(outsider.base:::is_docker_installed())
})
test_that('is_docker_running() works', {
  with_mock(
    `sys::exec_internal` = function(...) list('status' = 1),
    expect_false(outsider.base:::is_docker_running())
  )
  skip_if(!docker_available)
  expect_true(outsider.base:::is_docker_running())
})
test_that('docker_cmd() works', {
  skip_if(!docker_available)
  expect_true(outsider.base:::docker_cmd(args = '--help'))
})
test_that('docker_pull() works', {
  skip_if(!docker_available)
  expect_true(outsider.base:::docker_pull(img = 'hello-world'))
  on.exit(outsider.base:::docker_img_rm(img = 'hello-world'))
})
test_that('docker_build() and docker_img_rm() works', {
  skip_if(!docker_available)
  expect_false(outsider.base:::docker_build(img = img,
                                            url_or_path = 'notapath'))
  expect_true(outsider.base:::docker_build(img = img,
                                           url_or_path = dockerfile))
  expect_true(outsider.base:::docker_img_rm(img = img))
})
test_that('docker_cp() works', {
  skip_if(!docker_available)
  with_mock(
    `outsider.base:::docker_cmd` = function(...) TRUE,
    expect_true(outsider.base:::docker_cp(origin = '.', dest = '.'))
  )
})
test_that('docker_ps_count() works', {
  skip_if(!docker_available)
  expect_true(outsider.base:::docker_ps_count() == 0)
})
test_that('docker_img_ls() works', {
  skip_if(!docker_available)
  expctd_1 <- "REPOSITORY   TAG   IMAGE ID   CREATED   SIZE
dombennett/om_hello.world   latest   c0cb087733b9   1 day ago   100MB"
  expctd_2 <- "REPOSITORY   TAG   IMAGE ID   CREATED   SIZE"
  res <- with_mock(
    `sys::exec_internal` = function(...) list('status' = 0,
                                              'stdout' = charToRaw(expctd_1)),
    outsider.base:::docker_img_ls()
  )
  expect_true(nrow(res) == 1)
  res <- with_mock(
    `sys::exec_internal` = function(...) list('status' = 1,
                                              'stdout' = charToRaw(expctd_1)),
    outsider.base:::docker_img_ls()
  )
  expect_true(nrow(res) == 0)
  res <- with_mock(
    `sys::exec_internal` = function(...) list('status' = 0,
                                              'stdout' = charToRaw(expctd_2)),
    outsider.base:::docker_img_ls()
  )
  expect_true(nrow(res) == 0)
})
