# `travis-tool.sh install_deps` does not work for Windows Server 2019
# appveyor calls this script instead

# Vars ----
# github repo
repo <- 'AntonelliLab/outsider.base'

# Functions ----
read_deps <- function(repo) {
  url <- paste0('https://raw.githubusercontent.com/', repo,
                '/master/DESCRIPTION')
  lns <- readLines(con = url)
  strt <- which(grepl(pattern = 'Imports:', x = lns)) + 1
  deps <- lns[strt:length(lns)]
  deps <- deps[!grepl(pattern = 'Suggests:', x = deps)]
  deps <- sub(pattern = '\\(.*\\)', replacement = '', x = deps)
  deps <- gsub(pattern = '(\\s|,)', replacement = '', x = deps)
  deps
}

# Install
# deps <- read_deps(repo = repo)
# deps <- deps[deps != 'utils']
# for (dep in deps) {
#   cat('... Installing: [', dep, ']\n', sep = '')
#   utils::install.packages(pkgs = dep,
#                           repos = 'https://ftp.acc.umu.se/mirror/CRAN/')
# }
source('https://raw.githubusercontent.com/r-lib/remotes/master/install-github.R')$value('r-lib/remotes')
print("remotes" %in% rownames(installed.packages()))
