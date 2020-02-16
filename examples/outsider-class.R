\donttest{
# Set-up: install "hello.world", ships with ubuntu
# we can make simple commands in bash via R using the module
library(outsider.base)

# Manually install example module
# outsider.base contains the hello.world module in its package files
pkgnm <- 'om..hello.world'
mdl_flpth <- system.file('extdata', 'om..hello.world',
                         package = "outsider.base")
# install and import (outsider::module_install performs these tasks)
pkg_install(flpth = mdl_flpth)
image_install(pkgnm = pkgnm)

# Run echo
# create a outsider object that contains argument and Docker container details
otsdr <- outsider_init(pkgnm = pkgnm, cmd = 'echo', arglist = c('hello world!'))
# check details
print(otsdr)
# run the command
run(otsdr)

# Send a file
# an existing outsider object can be modified
tmppth <- tempdir()
flpth <- file.path(tmppth, 'testfile')
write(x = 'hello from within a file!', file = flpth)
otsdr$files_to_send <- flpth
otsdr$cmd <- 'cat'
otsdr$arglist <- 'testfile'
# check details
print(otsdr)
# run the command
run(otsdr)

# Return a file
# an existing outsider object can be modified
otsdr$files_to_send <- NULL
otsdr$cmd <- 'touch'
otsdr$arglist <- 'newfile'
otsdr$wd <- tmppth  # determines where created files are returned to
# check details
print(otsdr)
# run the command
run(otsdr)
# check if 'newfile' exists in tempdir()
nwflpth <- file.path(tmppth, 'newfile')
(file.exists(nwflpth))

# Clean-up
rm(otsdr)
file.remove(flpth)
file.remove(nwflpth)
uninstall(pkgnm)
}
