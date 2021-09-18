#====================#
#### Global Setup ####
#====================#

cat("\n>> [SETUP] Starting Global Setup ... <<\n")

if(!"renv" %in% utils::installed.packages()) {install.packages("renv"); library(renv)}

if(is.null(renv::project())) {
    renv::init(project = here::here(), bare = TRUE, restart = FALSE)
    file.create(here::here("config.yml"))
    cat('default:\r  data: !expr here::here("data", "my_data.csv")\r', file = here::here("config.yml"))
}

# TODO: usethis::use_blank_slate(scope = "project") <- only once ? no prompt ?

if(length(list.files(here::here("src", "common"))) == 0) system(here::here("update_commons.bat"), intern = TRUE)

source(here::here("src", "common", "packman.R"), echo = F)
init_base_packages()

source(here::here("src", "common", "utils.R"), echo = F)

source(here::here("src", "common", "config_global.R"), echo = F)

source(here::here("src", "common", "theme.R"), echo = F)

source(here::here("src", "setup_project.R"), echo = F)

cat(main("\n>> [SETUP] Global Setup DONE ! <<\n"))