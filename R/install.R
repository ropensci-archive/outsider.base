
#' @name is_installed
#' @title Is module installed?
#' @description Return TRUE if module is installed.
#' @param pkgnm Package name
#' @return logical(1)
#' @export
is_installed <- function(pkgnm) {
  installed <- modules_list()
  pkgnm %in% installed
}

#' @name install
#' @title Install module
#' @description Install outsider module: install R package, build/pull Docker
#' image. (Will only pull if image is available via DockerHub)
#' Returns 0 if all successful, 1 if only R package installs, 2 if R package
#' and Docker image both fail.
#' @param flpth File path to module directory.
#' @param tag Docker tag, default 'latest'
#' @param pull Pull from Docker Hub or build locally? Default, FALSE.
#' @param verbose Be verbose? Default TRUE.
#' @return Integer
#' @export
# TODO: uninstall if already exists?
install <- function(flpth, tag = 'latest', pull = FALSE,
                            verbose = TRUE) {
  success <- FALSE
  on.exit(expr = {
    if (!success) {
      uninstall(pkgnm = pkgnm)
    }
  })
  pkg <- devtools::as.package(x = flpth)
  pkgnm <- pkg[['package']]
  r_success <- devtools::install(pkg = pkg, force = TRUE, quiet = !verbose,
                                 reload = TRUE, build = FALSE)
  d_success <- image_install(pkgnm = pkgnm, tag = tag, pull = pull)
  success <- r_success & d_success
  res <- 2L - as.integer(r_success + d_success)
  invisible(res)
}

#' @name image_install
#' @title Install module's image
#' @description Install the Docker image for an outsider module after the module
#' package has been installed.
#' @param pkgnm Name of module's R package
#' @param tag Docker tag, default 'latest'
#' @param pull Pull from Docker Hub or build locally? Default, FALSE.
#' @return Integer
#' @export
image_install <- function(pkgnm, tag = 'latest', pull = TRUE) {
  success <- FALSE
  if (!is_installed(pkgnm = pkgnm)) {
    return(invisible(success))
  }
  img <- img_get(pkgnm = pkgnm)
  if (pull) {
    success <- docker_pull(img = img, tag = tag)
    if (!success) {
      msg <- paste0('Failed to pull ', char(paste0(img, ':', tag)),
                    'from Docker Hub. Will attempt to build locally instead.')
      warning(msg)
    }
  }
  if (!success) {
    dockerfile <- system.file('dockerfiles', tag, package = pkgnm)
    if (!dir.exists(dockerfile)) {
      msg <- paste0('No tag ', char(tag), ' for ', char(pkgnm))
      warning(msg)
    }
    success <- docker_build(img = img_get(pkgnm = pkgnm), tag = tag,
                            url_or_path = dockerfile)
  }
  success
}

#' @name uninstall
#' @title Uninstall and remove a module
#' @description Remove outsider module: uninstall package, delete Docker image.
#' @param pkgnm Package name
#' @details If program is successfully removed TRUE is returned, else FALSE.
#' @return Logical(1)
#' @export
# TODO: don't call error if no docker image
uninstall <- function(pkgnm) {
  if (is_installed(pkgnm = pkgnm)) {
    # TODO: are we sure this would remove all tagged version of an image?
    img <- img_get(pkgnm = pkgnm)
    try(docker_img_rm(img = img), silent = TRUE)
    devtools::uninstall(pkg = devtools::inst(pkgnm), quiet = TRUE,
                        unload = TRUE)
    }
  invisible(!is_installed(pkgnm = pkgnm))
}
