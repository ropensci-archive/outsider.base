exec_wait <- function(cmd, args = NULL, std_out = stdout(), std_err = stderr(), 
                      std_in = NULL, timeout = 0) {
  if (is_server_connected()) {
    session <- server_fetch(verbose = FALSE)
    command <- paste0(cmd, ' ', paste0(args, collapse = ' '))
    res <- ssh::ssh_exec_wait(session = session, command = command,
                              std_out = std_out, std_err = std_err)
  } else {
    res <- sys::exec_wait(cmd, args = args, std_out = std_out,
                          std_err = std_err, std_in = std_in, timeout = timeout)
  }
  res
}

exec_internal <- function(cmd, args = NULL, std_in = NULL, error = TRUE,
                          timeout = 0) {
  if (is_server_connected()) {
    session <- server_fetch(verbose = TRUE)
    command <- paste0(cmd, ' ', paste0(args, collapse = ' '))
    res <- ssh::ssh_exec_internal(session = session, command = command,
                                  error = error)
  } else {
    res <- sys::exec_internal(cmd = cmd, args = args, std_in = std_in,
                              error = error, timeout = timeout)
  }
  res
}