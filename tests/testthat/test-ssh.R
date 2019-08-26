context('Testing \'ssh\'')
session <- list('connected' = TRUE, 'user' = 'test_user',
                'host' = 'test_host')
test_that('server connection, fetch and disconnection functions work', {
  # connected?
  expect_false(outsider.base:::is_server_connected())
  # connect
  with_mock(
    `ssh::ssh_exec_wait` = function(...) 0,
    expect_true(server_connect(session = session))
  )
  # fetch
  with_mock(
    `ssh::ssh_info` = function(session) session,
    expect_equal(outsider.base:::server_fetch(verbose = TRUE), session)
  )
  # disconnect
  with_mock(
    `ssh::ssh_info` = function(session) session,
    `ssh::ssh_disconnect` = function(session) NULL,
    expect_true(server_disconnect())
  )
  # is still connected?
  expect_false(outsider.base:::is_server_connected())
})
context('Testing \'ssh\': copying files')
test_that('server_upload() works', {
  with_mock(
    `outsider.base:::server_fetch` = function(verbose) session,
    `ssh::scp_upload` = function(...) TRUE,
    expect_true(outsider.base:::server_upload(fl = 'test_file'))
  )
})
test_that('server_download() works', {
  # file
  with_mock(
    `outsider.base:::server_fetch` = function(verbose) session,
    `ssh::scp_download` = function(session, files, to, verbose) {
      print(files)
      print(to)
      file.create(file.path(to, 'downloaded_file'))
    },
    outsider.base:::server_download(origin = 'test_file', dest = 'test_file')
  )
  expect_true(file.exists('test_file'))
  file.remove('test_file')
  # folder
  with_mock(
    `outsider.base:::server_fetch` = function(verbose) session,
    `ssh::scp_download` = function(session, files, to, verbose) {
      print(files)
      print(to)
      dir.create(file.path(to, 'downloaded_folder'))
      file.create(file.path(to, 'downloaded_folder', 'test_file_1'))
      file.create(file.path(to, 'downloaded_folder', 'test_file_2'))
    },
    outsider.base:::server_download(origin = 'test_folder',
                                    dest = 'test_folder')
  )
  expect_true(dir.exists('test_folder'))
  expect_true(file.exists(file.path('test_folder', 'test_file_1')))
  unlink(x = 'test_folder', recursive = TRUE, force = TRUE)
})
test_that('upload/download/docker_cp work', {
  print_docker_cmd <- function(args, ...) {
    print(args)
    TRUE
  }
  # up
  with_mock(
    `outsider.base:::docker_cmd` = print_docker_cmd,
    `outsider.base:::is_server_connected` = function() TRUE,
    `outsider.base:::server_upload` = function(fl) {
      fl  == 'test_file'
      },
    expect_true(outsider.base:::docker_cp(origin = 'test_file',
                                          dest = 'c1:test_file'))
  )
  # down
  with_mock(
    `outsider.base:::docker_cmd` = print_docker_cmd,
    `outsider.base:::is_server_connected` = function() TRUE,
    `outsider.base:::server_download` = function(origin, dest) {
      origin == paste0(outsider.base:::ssh_wd, '/test_file') &
        dest  == 'test_file'
    },
    expect_true(outsider.base:::docker_cp(origin = 'c1:test_file',
                                          dest = 'test_file'))
  )
})
