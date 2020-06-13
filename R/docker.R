# Functions that interact directly with Docker

# Checks ----
#' @name is_docker_available
#' @title Check if Docker is installed and running
#' @description Raises an error if docker is not available.
#' @param call_error Call an error if no Docker detected? Default TRUE.
#' @return NULL
#' @export
is_docker_available <- function(call_error = TRUE) {
  installed <- is_docker_installed()
  if (!installed) {
    message(paste0('Docker is not installed. ',
                   'Follow the installation instructions for your system:\n',
                   'https://docs.docker.com/'))
    running <- FALSE
  } else {
    running <- is_docker_running()
    if (!running) {
      message(paste0('Docker is not running. ', 'Start the docker program by ',
                     'looking it up in your applications/programs and ',
                     'opening it.'))
    }
  }
  avlbl <- installed & running
  if (!avlbl & call_error) {
    stop("Docker is not available.", call. = FALSE)
  }
  invisible(avlbl)
}

#' @name is_docker_installed
#' @title Check if Docker is installed
#' @description Docker is required to run \code{outsider}. This function tests
#' whether Docker is installed.
#' @return Logical
#' @family private-check
is_docker_installed <- function() {
  success <- tryCatch(expr = {
    res <- exec_internal(cmd = 'docker', args = '--help', with_ssh=FALSE)
    res[['status']] == 0
  }, error = function(e) {
    FALSE
  })
  success
}

#' @name is_docker_running
#' @title Check if Docker is running
#' @description Docker is required to run \code{outsider}. This function tests
#' whether Docker is running.
#' @return Logical
#' @family private-check
is_docker_running <- function() {
  success <- tryCatch(expr = {
    res <- exec_internal(cmd = 'docker', args = 'ps', with_ssh=FALSE)
    res[['status']] == 0
  }, error = function(e) {
    FALSE
  })
  success
}

# Base function ----
#' @name docker_cmd
#' @title Run docker command
#' @description Runs a docker command with provided arguments
#' @param args Vector of arguments
#' @param std_out if and where to direct child process STDOUT.
#' See `sys::exec`.
#' @param std_err if and where to direct child process STDERR.
#' See `sys::exec`.
#' @return Logical
#' @family private-docker
docker_cmd <- function(args, std_out = TRUE, std_err = TRUE) {
  # cmd_args <- crayon::silver(paste0('docker ', paste(args, collapse = ' ')))
  # cat_line(crayon::bold('Command:\n'), cmd_args)
  # cat_line(crayon::silver(cli::rule(line = '.')))
  # TODO: cut down code duplication
  is_docker_available()
  if (!is_server_connected()) {
    callr_args <- list(exec_wait, args, std_out, std_err)
    res <- callr::r(func = function(exec_wait, args, std_out, std_err) {
      exec_wait(cmd = 'docker', args = args, std_out = std_out,
                std_err = std_err)
    }, args = callr_args, show = TRUE)
  } else {
    res <- exec_wait(cmd = 'docker', args = args, std_out = std_out,
                     std_err = std_err)
  }
  res == 0
}

# Derivatives ----
#' @name docker_img_rm
#' @title Remove docker image
#' @description Deletes docker image from system.
#' @param img Image name
#' @return Logical
#' @family private-docker
docker_img_rm <- function(img) {
  args <- c('image', 'rm', '--force', img)
  docker_cmd(args, std_out = log_get('docker_out'),
             std_err = log_get('docker_err'))
}

#' @name docker_pull
#' @title Pull an image from DockerHub.
#' @description Speeds up outsider module installation by downloading compiled
#' images.
#' @param img Image name
#' @param tag Tag version, default latest.
#' @return Logical
#' @family private-docker
docker_pull <- function(img, tag = 'latest') {
  args <- c('pull', paste0(img, ':', tag))
  docker_cmd(args = args, std_out = log_get('docker_out'),
             std_err = log_get('docker_err'))
}

#' @name docker_build
#' @title Build a docker image
#' @description Runs run \code{build} command.
#' @param img Image name
#' @param url_or_path Dockerfile URL
#' @param tag Docker tag, default 'latest'
#' @return Logical
#' @family private-docker
docker_build <- function(img, url_or_path, tag = 'latest') {
  args <- c('build', '-t', paste0(img, ':', tag), url_or_path)
  docker_cmd(args = args, std_out = log_get('docker_out'),
             std_err = log_get('docker_err'))
}

#' @name docker_cp
#' @title Copy files to and from container
#' @description Copy files to and from running Docker container
#' @details Container folders are indicated with
#' \code{[container_id]:[filepath]}.
#' Files are uploaded/downloaded to/from the server based on the presence of
#' ":" in origin/dest file paths.
#' @param origin Origin filepath
#' @param dest Destination filepath
#' @return Logical
#' @family private-docker
docker_cp <- function(origin, dest) {
  cp <- function(origin, dest) {
    args <- c('cp', origin, dest)
    docker_cmd(args = args, std_out = log_get('docker_out'),
               std_err = log_get('docker_err'))
  }
  server_fl_make <- function(fl) {
    server_fl <- sub(pattern = "^.*:", replacement = "", basename(fl))
    server_fl <- paste0(ssh_wd, '/', server_fl)
  }
  if (is_server_connected()) {
    if (!grepl(pattern = ':', x = origin)) {
      server_fl <- server_fl_make(fl = origin)
      # local machine -> server
      res1 <- server_upload(fl = origin)
      # server -> container
      res2 <- cp(origin = server_fl, dest = dest)
      res <- res1 & res2
    } else {
      server_fl <- server_fl_make(fl = dest)
      # container -> server
      res1 <- cp(origin = origin, dest = server_fl)
      # server -> local machine
      res2 <- server_download(origin = server_fl, dest = dest)
      res <- res1 & res2
    }
  } else {
    res <- cp(origin = origin, dest = dest)
  }
  res
}

# Special ----
#' @name docker_ps_count
#' @title Count docker processes
#' @description Count the number of running docker containers.
#' @details Use this to avoid creating multiple containers with the same ID.
#' @return Integer
#' @family private-docker
docker_ps_count <- function() {
  is_docker_available()
  res <- exec_internal(cmd = 'docker', args = 'ps')
  if (res[['status']] == 0) {
    ps <- strsplit(x = rawToChar(res[['stdout']]), split = '\n')[[1]][-1]
    return(length(ps))
  }
  0
}

#' @name docker_img_ls
#' @title List the number of installed images
#' @description Return a table of all the available Docker images.
#' @return tibble
#' @family docker
#' @export
docker_img_ls <- function() {
  res <- list()
  sys_out <- exec_internal(cmd = 'docker', args = c('image', 'ls'))
  if (sys_out[['status']] == 0) {
    images <- strsplit(x = rawToChar(sys_out[['stdout']]), split = '\n')[[1]]
    if (length(images) > 1) {
      images <- strsplit(x = images, split = '\\s{2,}')
      header <- gsub(pattern = ' ', replacement = '_',
                     x = tolower(images[[1]]))
      res <- matrix(data = unlist(images[-1]), nrow = length(images) - 1,
                    ncol = length(header), byrow = TRUE)
      colnames(res) <- header
    }
  }
  tibble::as_tibble(res)
}
