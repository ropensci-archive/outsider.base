context('Testing \'container\'')
test_that('container_init() works', {
  with_mock(
    `outsider.base:::docker_ids_get` = function(...) c('img' = img,
                                                       'cntnr' = cntnr,
                                                       'tag' = tag),
    expect_true(inherits(outsider.base:::container_init(pkgnm = pkgnm),
                         'container'))
  )
  with_mock(
    `outsider.base:::docker_ids_get` = function(...) c('img' = img,
                                                       'cntnr' = cntnr,
                                                       'tag' = tag),
    expect_true(inherits(outsider.base:::container_init(pkgnm = pkgnm),
                         'container'))
  )
  expect_error(outsider.base:::container_init())
})
test_that('run.container() works', {
  container <- structure(list(), class = 'container')
  with_mock(
    `outsider.base:::exec.container` = function(x, ...) TRUE,
    expect_true(outsider.base:::run.container(x = container, cmd = '',
                                              args = ''))
  )
  res <- with_mock(
    `outsider.base:::exec.container` = function(x, ...) stop(),
    outsider.base:::run.container(x = container, cmd = '', args = '')
  )
  expect_true(inherits(res, 'simpleError'))
})
test_that('print.container() works', {
   with_mock(
    `outsider.base:::docker_ids_get` = function(...) c('img' = img,
                                                       'cntnr' = cntnr,
                                                       'tag' = tag),
    `outsider.base:::status.container` = function(x, ...) 'This is a mock',
    container <- outsider.base:::container_init(pkgnm = pkgnm),
    expect_null(print(container))
  )
})
test_that('container methods work', {
  skip_if(!docker_available)
  # set-up
  outsider.base:::docker_build(img = img, url_or_path = dockerfile)
  container <- with_mock(
    `outsider.base:::docker_ids_get` = function(...) c('img' = img,
                                                       'cntnr' = cntnr,
                                                       'tag' = tag),
    outsider.base:::container_init(pkgnm = pkgnm)
  )
  # pull down
  on.exit(outsider.base:::docker_img_rm(img = img))
  # tests
  expect_true(outsider.base:::status.container(container) == 'Not running')
  expect_true(outsider.base:::start.container(container))
  expect_true(outsider.base:::exec.container(container, 'touch',
                                             'file_to_return'))
  expect_true(outsider.base:::copy.container(container, rtrn = getwd()))
  expect_true(file.exists('file_to_return'))
  expect_true(file.remove('file_to_return'))
  expect_true(file.create('file_to_send'))
  expect_true(outsider.base:::copy.container(container, send = 'file_to_send'))
  expect_true(file.remove('file_to_send'))
  #expect_true(outsider.base:::exec(container, 'ls'))
  expect_true(outsider.base:::status.container(container) == 'Running')
  expect_true(outsider.base:::halt.container(container))
})
