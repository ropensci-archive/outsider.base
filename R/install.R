
#' @name is_installed
#' @title Is module installed?
#' @description Return TRUE if module is installed.
#' @param pkgnm Package name
#' @return logical(1)
is_installed <- function(pkgnm) {
  installed <- modules_list()
  pkgnm %in% installed
}
installed_pkgs <- function(...) {
  utils::installed.packages(...)
}

#' @name install
#' @title Install module
#' @description Install outsider module: install R package, build/pull Docker
#' image.
#' @param flpth File path to module directory.
#' @param tag Docker tag, default 'latest'
#' @return logical
install <- function(flpth, tag = 'latest', pull = FALSE) {
  success <- FALSE
  on.exit(expr = {
    if (!success) {
      uninstall(pkgnm = pkgnm)
    }
  })
  # TODO: update quiet depending on log data
  pkgnm <- devtools::install(pkg = flpth, force = TRUE, quiet = TRUE,
                             reload = TRUE, build = FALSE)
  if (is_installed(pkgnm = pkgnm)) {
    if (pull) {
      success <- docker_pull(img = img_get(pkgnm = pkgnm), tag = tag)
    } else {
      dockerfile <- file.path(flpth, 'dockerfiles', tag)
      success <- docker_build(img = img_get(pkgnm = pkgnm),
                              url_or_path = dockerfile, tag = tag)
    }
  }
  invisible(success)
}

#' @name uninstall
#' @title Uninstall and remove a module
#' @description Remove outsider module: uninstall package, delete Docker image.
#' @param pkgnm Package name
#' @details If program is successfully removed from your system, TRUE is
#' returned else FALSE.
#' @return Logical(1)
#' @export
uninstall <- function(pkgnm) {
  if (is_installed(pkgnm = pkgnm)) {
    # TODO: are we sure this would remove all tagged version of an image?
    img <- meta_get(pkgnm = pkgnm)[['image']]
    try(docker_img_rm(img = img), silent = TRUE)
    devtools::uninstall(pkg = devtools::inst(pkgnm), quiet = TRUE,
                        unload = TRUE)
  }
  invisible(!is_installed(pkgnm = pkgnm))
}
