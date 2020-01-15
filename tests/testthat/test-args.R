context('Testing \'args\'')
test_that('to_basename() works', {
  expctd <- list.files(getwd())[1]
  args <- c(file.path(getwd(), expctd), 'arg1', 'arg2')
  expect_true(outsider.base:::to_basename(args)[1] == expctd)
})
test_that('is_filepath() works', {
  files <- list.files(getwd())
  expect_true(all(outsider.base:::is_filepath(files)))
})
test_that('arglist_get() works', {
  # TODO: why can't I specify an assigned variable as an argument?
  res <- arglist_get(outsider.base:::.packageName, 'b', '1')
  expect_equal(res, c(outsider.base:::.packageName, 'b', '1'))
  # check different depths
  foo <- function(...) {
    arglist_get(...)
  }
  res <- foo(outsider.base:::.packageName, 'b', '2')
  expect_equal(res, c(outsider.base:::.packageName, 'b', '2'))
  foo2 <- function(...) {
    foo(...)
  }
  res <- foo2(outsider.base:::.packageName, 'b', '3')
  expect_equal(res, c(outsider.base:::.packageName, 'b', '3'))
})
test_that('filestosend_get() works', {
  # nothin in, nothin out
  expect_equal(filestosend_get(character(0)), character())
  flnm <- 'testfile.txt'
  write('test', file = flnm)
  on.exit(file.remove(flnm))
  res <- filestosend_get(c('notafile', flnm))
  expect_true(res == flnm)
  res <- filestosend_get(c('notafile', flnm), wd = getwd())
  expect_true(file.path(getwd(), flnm) %in% res)
})
test_that('wd_get() works', {
  # nothin in, nothin out
  expect_equal(wd_get(character(0)), character())
  arglist <- c('1', '-wd', 'thisiswd/', '--otherarg')
  expect_equal(wd_get(arglist), getwd())
  expect_equal(wd_get(arglist, key = '-wd'), 'thisiswd/')
  arglist <- c('thisiswd/inputfile', '--otherarg', '--index', '1')
  expect_equal(wd_get(arglist, i = 1, key = '-wd'), 'thisiswd/inputfile')
  arglist <- c('inputfile', '--otherarg', '--index', '1', '-wd', 'thisiswd/')
  expect_equal(wd_get(arglist, i = 1, key = '-wd'), 'thisiswd/')
})
test_that('dirpath_get() works', {
  # nothin in, nothin out
  expect_equal(dirpath_get(character(0)), character())
  # if dirpath already, dirpath returned
  tmp <- tempdir()
  expect_equal(dirpath_get(tmp), tmp)
  # drop filename
  last8_get <- function(txt) {
    substr(x = txt, start = nchar(txt) - 8, stop = nchar(txt))
  }
  flnm <- 'afile.txt'
  test_flpth <- file.path(tmp, flnm)
  res_flpth <- dirpath_get(test_flpth)
  expect_true(last8_get(test_flpth) == flnm)
  expect_true(last8_get(res_flpth) != flnm)
})
test_that('arglist_parse() works', {
  # nothin in, nothin out
  expect_equal(arglist_parse(character(0)), character())
  # path normalisation
  tmp <- file.path(tempdir(), 'data')
  dir.create(tmp)
  on.exit(unlink(tmp))
  res <- arglist_parse(arglist = tmp)
  expect_equal(res, 'data')
  res <- arglist_parse(arglist = 'not/a/real/file/path')
  expect_equal(res, 'not/a/real/file/path')
  # drop keyvals
  res <- arglist_parse(arglist = c('-wd', 'thisiswd/', '-verbosity', '2',
                                    'otherarg'),
                        keyvals_to_drop = c('-wd', '-verbosity'))
  expect_equal(res, 'otherarg')
  # drop vals
  res <- arglist_parse(arglist = c('-wd', 'thisiswd/', '-verbosity', '2',
                                    'otherarg', '--unwanted1', '--unwanted2'),
                        keyvals_to_drop = c('-wd', '-verbosity'),
                        vals_to_drop = c('--unwanted1', '--unwanted2'))
  expect_equal(res, 'otherarg')
})
