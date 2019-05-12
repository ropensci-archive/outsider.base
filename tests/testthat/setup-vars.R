# shared vars
pkgnm <- 'om..hello.world'
img <- 'dombennett/om_hello_world'
meta_img <- 'om_hello_world' # as it appears in om.yml
cntnr <- 'om__hello_world_1'
tag <- 'latest'
# mdl_flpth
mdl_flpth <- system.file('extdata', 'om..hello.world',
                         package = "outsider.base")
if (dir.exists(mdl_flpth)) {
  cat(crayon::bgGreen(crayon::white('Found `om..hello.world`!')), '\n')
} else {
  cat(crayon::bgRed(crayon::white('Did NOT find `om..hello.world`!')), '\n')
}
if (!dir.exists(file.path(mdl_flpth, 'inst'))) {
  cat(crayon::bgRed(crayon::white('Did NOT find `inst/`!')), '\n')
}
dockerfile <- file.path(mdl_flpth, 'inst', 'dockerfiles', 'latest')
if (is_installed(pkgnm = pkgnm)) {
  uninstall(pkgnm = pkgnm)
}
