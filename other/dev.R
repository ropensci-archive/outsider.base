# TODO:
# - look through code and check for inconsistencies

session <- ssh::ssh_connect(host = "dom@130.241.157.50:6582")
server_connect(session)
outsider.base:::docker_cmd(args = c('run', 'hello-world'))
