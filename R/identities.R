
#' @name modules_list
#' @title List all installed outsider modules
#' @description Return the image and container names for a module
#' @param pkgnm Package name of module
#' @return Logical
#' @family ids
modules_list <- function() {
  pkgs <- installed_pkgs()
  pkgnms <- unname(pkgs[, 'Package'])
  libpaths <- unname(pkgs[, 'LibPath'])
  yamls <- file.path(libpaths, pkgnms, 'om.yml')
  pkgnms[file.exists(yamls)]
}

# TODO: move to outsider?
#' @name guess_pkgnm
#' @title Guess package name
#' @description Return package name from a repo name.
#' @param repo Repository (e.g. GitHub) associated with module
#' @details Raises error if no module discovered.
#' @return character(1)
#' @family ids
guess_pkgnm <- function(repo) {
  mdls <- modules_list()
  metas <- lapply(X = mdls, FUN = meta_get)
  # TODO: expand for more detection
  pull <- vapply(X = metas, FUN = function(x) {
    !is.null(x[['url']]) && grepl(pattern = repo, x = x[['url']],
                                  ignore.case = TRUE)
  }, FUN.VALUE = logical(1))
  if (sum(pull) == 1) {
    res <- mdls[pull]
  } else {
    stop(paste0('No module associated with ', char(repo), ' could be found.'),
         call. = FALSE)
  }
  res
}

#' @name meta_get
#' @title Get outsider module details
#' @description Return a named list of all metadata associated with a module
#' @param pkgnm Package name of module
#' @return Named list
#' @family ids
meta_get <- function(pkgnm) {
  res <- yaml::read_yaml(file = system.file('om.yml', package = pkgnm))
  res[['image']] <- gsub(pattern = '\\.', replacement = '_', x = pkgnm)
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
  if (is.null(meta[['docker']])) {
    res <- meta[['image']]
  } else {
    res <- paste0(meta[['docker']], '/', meta[['image']])
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
  #print(imgs)
  if ('tag' %in% colnames(imgs)) {
    pull <- imgs[['repository']] == img_get(pkgnm)
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
