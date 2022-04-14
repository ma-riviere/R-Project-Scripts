#====================#
#### Project Init ####
#====================#

cat("\n[SETUP] Starting Project init ...\n")

if(!"renv" %in% utils::installed.packages()) {install.packages("renv"); library(renv)}

if(is.null(renv::project())) {
  renv::init(project = here::here(), bare = TRUE, restart = FALSE)
  if(!file.exists(here::here("config.yml"))) {
    file.create(here::here("config.yml"))
    cat('default:\r  data: !expr here::here("data", "my_data.csv")\r', file = here::here("config.yml"))
  }
  # usethis::use_blank_slate(scope = "project") ## TODO: only once + no prompt ?
}

if(!file.exists(here::here("secret.yml"))) {
  file.create(here::here("secret.yml"))
  cat('default:\r  api_key: ""\r', file = here::here("secret.yml"))
}

com_path <- here::here("src", "common")

source(here::here(com_path, "logger.R"), echo = F)
source(here::here(com_path, "utils.R"), echo = F)
source(here::here("src", "packages.R"), echo = F)
source(here::here(com_path, "packman.R"), echo = F)
init_base_packages()

source(here::here(com_path, "config_global.R"), echo = F)
global_config <- load_global_config()


#--------------------------------#
#### Project-specific scripts ####
#--------------------------------#

setup_project <- function(...) {
  
  source(here::here("src", "authors.R"), echo = F)

  init_project_packages(...)

  source(here::here("src", "config_project.R"), echo = F)
  
  log.main("[SETUP] Loading additional src scripts ...")
  
  project_scrips <- fs::dir_ls(path = here::here("src"), type = "file", glob = "*.R") |> fs::path_file()  # Loading data.R & co

  sapply(
    project_scrips[which(project_scrips %ni% c("packages.R", "config_project.R"))],
    FUN = \(f) source(here::here("src", f), echo = F)
  )
}