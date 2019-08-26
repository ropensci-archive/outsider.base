context('Testing \'sys\'')
test_that('exec_wait() works', {
  with_mock(
    `outsider.base:::is_server_connected` = function() FALSE,
    `sys::exec_wait` = function(...) TRUE,
    expect_true(outsider.base:::exec_wait(cmd = 'test'))
  )
  with_mock(
    `outsider.base:::is_server_connected` = function() TRUE,
    `outsider.base:::server_fetch` = function(verbose) 'session_obj',
    `ssh::ssh_exec_wait` = function(...) TRUE,
    expect_true(outsider.base:::exec_wait(cmd = 'test'))
  )
})
test_that('exec_internal() works', {
  with_mock(
    `outsider.base:::is_server_connected` = function() FALSE,
    `sys::exec_internal` = function(...) TRUE,
    expect_true(outsider.base:::exec_internal(cmd = 'test'))
  )
  with_mock(
    `outsider.base:::is_server_connected` = function() TRUE,
    `outsider.base:::server_fetch` = function(verbose) 'session_obj',
    `ssh::ssh_exec_internal` = function(...) TRUE,
    expect_true(outsider.base:::exec_internal(cmd = 'test'))
  )
})
