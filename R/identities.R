
#' @name modules_list
#' @title List all installed outsider modules
#' @description Return the R package names of all installed outsider modules
#' @return Logical
#' @family ids
#' @export
modules_list <- function() {
  pkgs <- installed_pkgs()
  pkgnms <- unname(pkgs[, 'Package'])
  libpaths <- unname(pkgs[, 'LibPath'])
  yamls <- file.path(libpaths, pkgnms, 'om.yml')
  pkgnms[file.exists(yamls)]
}
installed_pkgs <- function(...) {
  utils::installed.packages(...)
}

#' @name meta_get
#' @title Get outsider module details
#' @description Return a named list of all metadata associated with a module
#' @param pkgnm Package name of module
#' @return Named list
#' @family ids
#' @export
meta_get <- function(pkgnm) {
  yml_flpth <- system.file('om.yml', package = pkgnm)
  if (!file.exists(yml_flpth)) {
    msg <- paste0('No ', char('om.yml'), ' for ', char(pkgnm))
    stop(msg, call. = FALSE)
  }
  res <- yaml::read_yaml(file = yml_flpth)
  names(res) <- tolower(names(res))
  # . are not allowed in image names
  res[['image']] <- gsub(pattern = '\\.+', replacement = '_', x = pkgnm)
  # container_base is based off image
  res[['container']] <- res[['image']]
  if (!is.null(res[['docker']]) && nchar(res[['docker']]) > 1) {
    res[['image']] <- paste0(res[['docker']], '/', res[['image']])
  }
  res[['package']] <- pkgnm
  services <- c('github', 'gitlab', 'bitbucket')
  if (!'url' %in% names(res) & any(services %in% names(res))) {
    service <- services[services %in% names(res)][[1]]
    url <- switch(service, github = 'https://github.com/',
                  gitlab = 'https://gitlab.com/',
                  bitbucket = 'https://bitbucket.org/')
    res[['url']] <- paste0(url, res[["package"]])
  }
  res
}

#' @name docker_ids_get
#' @title Get docker names for a module
#' @description Return the image and container names for a module. Will attempt
#' to build/pull image if missing.
#' @param pkgnm Package name of module
#' @return Logical
#' @family ids
docker_ids_get <- function(pkgnm) {
  meta <- meta_get(pkgnm = pkgnm)
  nps <- docker_ps_count()
  imgs <- docker_img_ls()
  img <- meta[['image']]
  pull <- imgs[['repository']] == img
  if (!any(pull)) {
    # image is missing, false install
    msg <- paste0('No Docker image found for ', char(pkgnm),
                  ' -- attempting to pull/build image with tag ',
                  char('latest'))
    message(msg)
    success <- image_install(pkgnm = pkgnm, tag = 'latest', pull = TRUE)
    if (!success) {
      stop('Failed to build/pull image.', call. = FALSE)
    }
    imgs <- docker_img_ls()
    pull <- imgs[['repository']] == img
  }
  if ('tag' %in% colnames(imgs)) {
    tag <- imgs[pull, 'tag'][[1]]
    tag <- tag[[1]]
  } else {
    # Sometimes there is no tag column (?)
    # Most of the time it should be 'latest'
    tag <- 'latest'
    msg <- paste0('No tags detected, using ', char(tag))
    warning(msg)
  }
  cntnr <- paste0(meta[['container']], '_', nps)
  c('img' = img, 'cntnr' = cntnr, 'tag' = tag)
}
