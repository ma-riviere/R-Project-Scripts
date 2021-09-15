#=============================#
#### Global project config ####
#=============================#

if(!"renv" %in% utils::installed.packages()) install.packages("renv")
renv::init()

source("packman.R", echo = F)

common_pkgs <- c("config", "magrittr", "remotes", "crayon", "knitr", "rmarkdown", "glue", "styler", "miniUI", "tools", "usethis", "rlang")

install_packages(common_pkgs)

main <- crayon::magenta$bold
note <- crayon::blue
error <- crayon::red
warn <- crayon::yellow

cat(main("\n[PACKAGES] Base packages installed.\n\n"))

source("utils.R", echo = F)

source("config.R", echo = F)

source("viz.R", echo = F)