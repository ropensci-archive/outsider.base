context('Testing \'console\'')
test_that('.onAttach() works', {
  expect_true(outsider.base:::.onAttach())
})
test_that('char() works', {
  expect_true(is.character(outsider.base:::char('char')))
})
test_that('stat() works', {
  expect_true(is.character(outsider.base:::stat('stat')))
})
test_that('cat_line() works', {
  expect_null(outsider.base:::cat_line('cat this'))
})
