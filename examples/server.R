library(outsider.base)

# NOT RUN
\dontrun{
session <- ssh::ssh_connect(host = '[INSERT HOST IP]')
server_connect(session = session)
# run outsider.base commands, when finished
server_disconnect()
}
