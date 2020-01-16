# Vars ----
ssh_wd <- '.outsider_workdir'
readme_text <- "outsider working directory. This folder contains files/folders
sent to this machine\'s Docker.

For more information visit, https://docs.ropensci.org/outsider"

# Public ----
#' @name server_connect
#' @title Connect to a server
#' @description Connect to a server, make accessible to \code{outsider} and
#' set-up for \code{outsider} interaction.
#' @return logical
#' @param session ssh session, see \code{\link[ssh]{ssh_connect}}
#' @family public-server
#' @example examples/server.R
#' @export
server_connect <- function(session) {
  if (!requireNamespace("ssh", quietly = TRUE)) {
    msg <- paste0("Package ", char("ssh"), " required. Run ",
                  char("install.packages(\"ssh\")"), ' or similar.')
    stop(msg, call. = FALSE)
  }
  # set in options()
  options('outsider-ssh-session' = session)
  # create working dir (assumes a UNIX system)
  command <- c(paste0("if [ ! -e ", ssh_wd, " ];\nthen mkdir ", ssh_wd, "\nfi"),
               paste0("echo \"", readme_text, '\" > ', ssh_wd, '/README'))
  res <- ssh::ssh_exec_wait(session = session, command = command)
  invisible(res == 0)
}

#' @name server_disconnect
#' @title Disconnect from a server
#' @description Disconnect from a server and remove from \code{outsider}
#' @return logical
#' @family public-server
#' @example examples/server.R
#' @export
server_disconnect <- function() {
  if (is_server_connected()) {
    ssh::ssh_disconnect(getOption(x = 'outsider-ssh-session'))
    options('outsider-ssh-session' = NULL)
  }
  invisible(TRUE)
}

# Private ----
#' @name is_server_connected
#' @title Is server connected?
#' @description Return TRUE if an \code{ssh} session exists with which
#' \code{outsider} can interact.
#' @return logical
#' @family private-server
is_server_connected <- function() {
  'outsider-ssh-session' %in% names(options()) &&
    ssh::ssh_info(getOption(x = 'outsider-ssh-session'))[['connected']]
}

#' @name server_fetch
#' @title Fetch server "session"
#' @description Return connected session to server.
#' @details See \code{\link[ssh]{ssh_connect}} for more details.
#' @param verbose Be verbose? Logical.
#' @return ssh session
#' @family private-server
server_fetch <- function(verbose) {
  session <- getOption(x = 'outsider-ssh-session')
  if (verbose) {
    info <- ssh::ssh_info(session)
    msg <- paste0('Running commands at: ', info[['user']], '@', info[['host']])
    message((crayon::silver(msg)))
  }
  session
}

#' @name server_upload
#' @title Upload to server
#' @description Upload file/folder to connected server. File is placed in
#' working dir on server.
#' @param fl File/folder to be transferred.
#' @return Logical
#' @family private-server
server_upload <- function(fl) {
  # TODO: ensure windows files are suitable for linux?
  session <- server_fetch(verbose = FALSE)
  ssh::scp_upload(session = session, files = fl, to = ssh_wd,
                  verbose = log_get('docker_out'))
  invisible(TRUE)
}

#' @name server_download
#' @title Download from server
#' @description Download file/folder from connected server. File is copied to
#' a temporary folder before transferred to desired destination.
#' @param origin Origin filepath
#' @param dest Destination filepath
#' @return Logical
#' @family private-server
server_download <- function(origin, dest) {
  session <- server_fetch(verbose = FALSE)
  # create temp dir to host transferred file
  # (difficult to work with filepaths if remote and local machine have
  # different OSs)
  tmp_flpth <- file.path(tempdir(), 'outsider_ssh_files')
  if (!dir.exists(tmp_flpth)) {
    dir.create(tmp_flpth)
  }
  on.exit(unlink(x = tmp_flpth, recursive = TRUE, force = TRUE))
  ssh::scp_download(session = session, files = origin, to = tmp_flpth,
                    verbose = log_get('docker_out'))
  fl <- file.path(tmp_flpth, list.files(tmp_flpth))
  if (length(fl) > 1) {
    stop('More files than expected.')
  }
  if (dir.exists(fl)) {
    if (!dir.exists(dest)) {
      dir.create(dest)
    }
    for (subfl in list.files(fl)) {
      file.copy(from = file.path(fl, subfl), to = file.path(dest, subfl))
    }
  } else {
    file.copy(from = fl, to = dest)
  }
  invisible(file.exists(dest))
}

