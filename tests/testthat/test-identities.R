context('Testing \'identities\'')
test_that('modules_list() works', {
  res <- outsider.base::modules_list()
  expect_type(res, 'character')
})
test_that('meta_get() works', {
  # TOOD: add mock for system.file
  expect_error(outsider.base:::meta_get('outsider.base'))
})
test_that('docker_ids_get() works', {
  meta <- list('image' = img, 'docker' = 'dombennett')
  # wo/ tag info
  imgs <- tibble::as_tibble(list('repository' = img))
  res <- with_mock(
    `outsider.base::meta_get` = function(...) meta,
    `outsider.base:::docker_ps_count` = function(...) 0,
    `outsider.base::docker_img_ls` = function(...) imgs,
    expect_warning(outsider.base:::docker_ids_get(pkgnm = pkgnm))
  )
  expect_true(all(names(res) %in% c('img', 'cntnr', 'tag')))
  # w/ tag info
  imgs <- tibble::as_tibble(list('repository' = img, 'tag' = 'latest'))
  res <- with_mock(
    `outsider.base:::meta_get` = function(...) meta,
    `outsider.base:::docker_ps_count` = function(...) 0,
    `outsider.base:::docker_img_ls` = function(...) imgs,
    outsider.base:::docker_ids_get(pkgnm = pkgnm)
  )
  expect_true(all(names(res) %in% c('img', 'cntnr', 'tag')))
  # auto-build/pull
  imgs <- tibble::as_tibble(list())
  with_mock(
    `outsider.base:::meta_get` = function(...) meta,
    `outsider.base:::docker_ps_count` = function(...) 0,
    `outsider.base:::docker_img_ls` = function(...) imgs,
    `outsider.base::image_install` = function(...) FALSE,
    expect_error(outsider.base:::docker_ids_get(pkgnm = pkgnm))
  )
})
