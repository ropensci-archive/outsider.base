context('Testing \'identities\'')
test_that('modules_list() works', {
  res <- outsider.base::modules_list()
  expect_type(res, 'character')
})
test_that('meta_get() works', {
  # TOOD: add mock for system.file
  expect_error(outsider.base:::meta_get('outsider.base'))
})
test_that('img_get() works', {
  foo <- function(meta) {
    with_mock(
      `outsider.base:::meta_get` = function(...) meta,
      outsider.base:::img_get('pkg')
    )
  }
  # works with and without docker username
  res <- foo(meta = list('docker' = 'an', 'image' = 'img'))
  expect_true(res == 'an/img')
  res <- foo(meta = list('image' = 'img'))
  expect_true(res == 'img')
})
test_that('docker_ids_get() works', {
  # wo/ tag info
  imgs <- tibble::as_tibble(list('repository' = img))
  res <- with_mock(
    `outsider.base:::meta_get` = function(...) list('image' = meta_img),
    `outsider.base:::docker_ps_count` = function(...) 0,
    `outsider.base:::docker_img_ls` = function(...) imgs,
    outsider.base:::docker_ids_get(pkgnm = pkgnm)
  )
  expect_true(all(names(res) %in% c('img', 'cntnr', 'tag')))
  # w/ tag info
  imgs <- tibble::as_tibble(list('repository' = img, 'tag' = 'latest'))
  res <- with_mock(
    `outsider.base:::meta_get` = function(...) list('image' = img),
    `outsider.base:::docker_ps_count` = function(...) 0,
    `outsider.base:::docker_img_ls` = function(...) imgs,
    outsider.base:::docker_ids_get(pkgnm = pkgnm)
  )
  expect_true(all(names(res) %in% c('img', 'cntnr', 'tag')))
})
