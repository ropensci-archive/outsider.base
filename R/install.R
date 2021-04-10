
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

#' @name pkg_install
#' @title Install outsider module package
#' @description Install outsider module's package.
#' @param flpth File path to module directory.
#' @param verbose Be verbose? Default TRUE.
#' @return Logical(1)
#' @export
pkg_install <- function(flpth, verbose = TRUE) {
  pkg <- devtools::as.package(x = flpth)
  res <- devtools::install(pkg = pkg, force = TRUE, quiet = !verbose,
                           reload = TRUE, build = FALSE)
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
  img <- meta_get(pkgnm = pkgnm)[['image']]
  if (pull & grepl(pattern = '/', x = img)) {
    success <- docker_pull(img = img, tag = tag)
    if (!success) {
      msg <- paste0('Failed to pull ', char(paste0(img, ':', tag)),
                    'from Docker Hub. Will attempt to build locally instead.')
      warning(msg)
    }
  }
  if (!success) {
    # TODO: what if building remotely?
    dockerfile <- system.file('dockerfiles', tag, package = pkgnm)
    if (!dir.exists(dockerfile)) {
      msg <- paste0('No tag ', char(tag), ' for ', char(pkgnm))
      warning(msg)
    }
    success <- docker_build(img = img, tag = tag,
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
    img <- meta_get(pkgnm = pkgnm)[['image']]
    try(docker_img_rm(img = img), silent = TRUE)
    devtools::uninstall(pkg = pkgload::inst(pkgnm), quiet = TRUE,
                        unload = TRUE)
    }
  invisible(!is_installed(pkgnm = pkgnm))
}
