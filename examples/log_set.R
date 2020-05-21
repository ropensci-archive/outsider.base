\donttest{
library(outsider.base)

# Manually install example module
# outsider.base contains the hello.world module in its package files
pkgnm <- 'om..hello.world'
mdl_flpth <- system.file('extdata', 'om..hello.world',
                         package = "outsider.base")
# install and import (outsider::module_install performs these tasks)
pkg_install(flpth = mdl_flpth)
image_install(pkgnm = pkgnm)
# (outsider::module_import performs this task)
hello_world <- utils::getFromNamespace(x = 'hello_world', ns = pkgnm)

# control the log stream
# send output to file
tmpfl <- tempfile()
log_set(log = 'program_out', val = tmpfl)
hello_world()
(readLines(con = tmpfl))
file.remove(tmpfl)
# send docker and program output to console
log_set(log = 'program_out', val = TRUE)
log_set(log = 'docker_out', val = TRUE)
hello_world()

# clean-up
uninstall(pkgnm)
}
