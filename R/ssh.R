# # TODO: allow servers
# 
# # if (!requireNamespace("ssh", quietly = TRUE)) {
# #   msg <- paste0("Package ", char("ssh"), " required. Run ",
# #                 elem("install.packages(\"ssh\")"), ' or similar.')
# #   stop(msg, call. = FALSE)
# # }
# 
# option_nm <- 'outsider-ssh-session'
# 
# server_connect <- function(session) {
#   # set in options()
#   options(option_nm = session)
#   # create working dir
#   invisible(TRUE)
# }
# 
# server_disconnect <- function() {
# 
# }
# 
# is_server_connected <- function() {
#   option_nm %in% names(options()) &&
#     ssh::ssh_info(getOption(x = option_nm))[['connected']]
# }
# 
# server_fetch <- function() {
#   session <- getOption(x = option_nm)
#   info <- ssh::ssh_info(session)
#   paste0(info[['user']]
# }
