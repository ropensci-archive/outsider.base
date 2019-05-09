
#' @name modules_list
#' @title List all installed outsider modules
#' @description Return the image and container names for a module
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
  # . are not allowed in image names
  res[['image']] <- gsub(pattern = '\\.+', replacement = '_', x = pkgnm)
  res[['package']] <- pkgnm
  res
}

#' @name img_get
#' @title Construct Docker image name from meta
#' @description Return Docker image name using DockerHub username, if available.
#' @param pkgnm Package name of module
#' @return character(1)
#' @family ids
img_get <- function(pkgnm) {
  meta <- meta_get(pkgnm = pkgnm)
  if ('docker' %in% names(meta)) {
    res <- paste0(meta[['docker']], '/', meta[['image']])
  } else {
    res <- meta[['image']]
  }
  res
}

#' @name docker_ids_get
#' @title Get docker names for a module
#' @description Return the image and container names for a module
#' @param pkgnm Package name of module
#' @return Logical
#' @family ids
docker_ids_get <- function(pkgnm) {
  meta <- meta_get(pkgnm = pkgnm)
  nps <- docker_ps_count()
  imgs <- docker_img_ls()
  img <- img_get(pkgnm)
  #print(imgs)
  if ('tag' %in% colnames(imgs)) {
    pull <- imgs[['repository']] == img
    if (any(pull)) {
      tag <- imgs[pull, 'tag'][[1]]
      tag <- tag[[1]]
    } else {
      stop(char(pkgnm), ' is missing its Docker image, try reinstalling.')
    }
  } else {
    # Sometimes there is no tag column (?)
    tag <- 'latest'
  }
  cntnr <- paste0(meta[['image']], '_', nps)
  c('img' = img, 'cntnr' = cntnr, 'tag' = tag)
}
