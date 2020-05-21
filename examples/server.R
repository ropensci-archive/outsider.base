library(outsider.base)

# NOT RUN
\dontrun{
if (requireNamespace("ssh", quietly = TRUE)) {
  session <- ssh::ssh_connect(host = '[INSERT HOST IP]')
  server_connect(session = session)
  # run outsider.base commands, when finished
  server_disconnect()
}
}
