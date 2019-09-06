#' @name exec_wait
#' @title Execute system commands and wait for response
#' @description Passes arguments to \code{\link[sys]{exec_wait}}, if a server
#' is connected arguments are passed to \code{\link[ssh]{ssh_exec_wait}}
#' instead.
#' @param cmd Command
#' @param args Arguments
#' @param std_out Standard out
#' @param std_err Standard error
#' @param std_in Standard in
#' @param timeout Timeout
#' @return logical
#' @family private-sys
exec_wait <- function(cmd, args = NULL, std_out = stdout(), std_err = stderr(), 
                      std_in = NULL, timeout = 0) {
  if (is_server_connected()) {
    session <- server_fetch(verbose = 'exec' %in% args)
    command <- paste0(cmd, ' ', paste0(args, collapse = ' '))
    res <- ssh::ssh_exec_wait(session = session, command = command,
                              std_out = std_out, std_err = std_err)
  } else {
    res <- sys::exec_wait(cmd, args = args, std_out = std_out,
                          std_err = std_err, std_in = std_in, timeout = timeout)
  }
  res
}

#' @name exec_internal
#' @title Execute system commands and wait for response
#' @description Passes arguments to \code{\link[sys]{exec_internal}}, if a
#' server is connected arguments are passed to
#' \code{\link[ssh]{ssh_exec_internal}} instead.
#' @param cmd Command
#' @param args Arguments
#' @param std_in Standard in
#' @param error Call an error? T/F
#' @param timeout Timeout
#' @return logical
#' @family private-sys
exec_internal <- function(cmd, args = NULL, std_in = NULL, error = TRUE,
                          timeout = 0) {
  if (is_server_connected()) {
    session <- server_fetch(verbose = FALSE)
    command <- paste0(cmd, ' ', paste0(args, collapse = ' '))
    res <- ssh::ssh_exec_internal(session = session, command = command,
                                  error = error)
  } else {
    res <- sys::exec_internal(cmd = cmd, args = args, std_in = std_in,
                              error = error, timeout = timeout)
  }
  res
}
