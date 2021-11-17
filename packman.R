#========================#
#### Packages Manager ####
#========================#

base_pkgs <- c("renv", "here", "config", "gert", "knitr", "rmarkdown", "ggplot2", "crayon", "usethis")
suite_pkgs_names <- c("tidyverse", "tidymodels", "easystats")

options(
  pkgType = ifelse(Sys.info()[["sysname"]] == "Windows", "both", "source"),
  Ncpus = max(1, parallel::detectCores(logical = TRUE) - 1),
  # install.packages.check.source = "no",
  verbose = FALSE
)

Sys.setenv(MAKEFLAGS = paste0("-j", getOption("Ncpus")))

#---------------------#
#### Main function ####
#---------------------#

init_project_packages <- function(update = FALSE, clean = TRUE) {
    
  if(update) {

    if(clean) {
      cat(note("\n[PACKAGES] Cleaning illegal project packages ...\n"))
      renv::clean(prompt = FALSE)
    }
    
    cat(note("\n[PACKAGES] Updating submodules ...\n"))
    update_submodules()
    
    cat(note("\n[PACKAGES] Configuring GITHUB access ...\n"))
    configure_git()

    options(repos = project_repos)
    
    cat(note("\n[PACKAGES] Installing project packages ...\n"))
    install_packages(project_pkgs)
    
    cat(note("\n[PACKAGES] Loading project packages ...\n"))
    load_packages(project_pkgs)
    
    cat(note("\n[PACKAGES] Indexing project packages ...\n\n"))
    if(file.exists(here::here("DESCRIPTION"))) file.remove(here::here("DESCRIPTION"))
    
    usethis::use_description(
      fields = list(
        `Authors@R` = 'person("Marc-Aurele", "RIVIERE", email = "ma.riviere987@gmail.com",
          role = c("aut", "cre"),
          comment = c(ORCID = "0000-0002-5108-3382", GITHUB = "https://github.com/ma-riviere"))',
        Title = "_",
        Description = "_",
        Language =  "en"
      ),
      roxygen = FALSE,
      check_name = FALSE
    )
    usethis::use_mit_license("Marc-Aurele RIVIERE")
    add_packages_to_description(c(base_pkgs, project_pkgs))
    
    ## Updating renv.lock
    renv::snapshot(type = "explicit", prompt = FALSE)
    
  } else {
    cat(note("\n[PACKAGES] Restoring project packages ...\n"))
    renv::restore(prompt = FALSE)
    load_packages(project_pkgs)
  }
  
  cat(note("\n[PACKAGES] Configuring project's packages ...\n"))
  configure_packages()
}

init_base_packages <- function() {
  install_packages(base_pkgs)
  
  main <<- crayon::magenta$bold
  note <<- crayon::blue
  warn <<- crayon::yellow
  error <<- crayon::red
    
  load_packages(base_pkgs)
  
  cat(main("\n[PACKAGES] Base packages installed.\n"))
}

#------------------------#
#### Helper functions ####
#------------------------#

get_pkg_name <- function(pkg) {
  pkg_name <- pkg
  if (grepl("/", pkg_name, fixed = TRUE)) {
    pkg_path <- strsplit(pkg_name, "/", fixed = TRUE)[[1]]
    pkg_name <- pkg_path[length(pkg_path)]
  }
  if (grepl("@", pkg_name, fixed = TRUE)) {
    pkg_path <- strsplit(pkg_name, "@", fixed = TRUE)[[1]]
    pkg_name <- pkg_path[1]
  }
  return(pkg_name)
}

get_pkg_version <- function(pkg) {
  if (grepl("@", pkg, fixed = TRUE)) {
    pkg_path <- strsplit(pkg, split = "@", fixed = TRUE)[[1]]
    return(pkg_path[length(pkg_path)])
  }
  return("0.0.0")
}

get_renv_installed_pkgs <- function() {
  return(list.dirs(renv::paths$library(), full.names = F, recursive = F))
}

is_installed <- function(pkg) {
  return(pkg %in% get_renv_installed_pkgs())
}

should_install <- function(pkg) {
  pkg_name <- get_pkg_name(pkg)
  if (is_installed(pkg_name)) {
    if(get_pkg_version(pkg) != "0.0.0" && utils::packageVersion(pkg_name) != get_pkg_version(pkg)) return(TRUE)
    return(FALSE) 
  }
  return(TRUE)
}

install_packages <- function(pkgs) {
  suppressPackageStartupMessages({
    for (pkg in pkgs) {
      if (should_install(pkg)) renv::install(packages = pkg, prompt = FALSE, build_vignettes = FALSE)
    }
  })
}

load_packages <- function(pkgs) {
  suppressPackageStartupMessages({
    for (pkg in pkgs) {
      pkg_name <- get_pkg_name(pkg)
      if (is_installed(pkg_name)) require(pkg_name, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    }
  })
}

add_packages_to_description <- function(pkgs) {
  for (pkg in pkgs) {
    pkg_name <- get_pkg_name(pkg)
    if (grepl("/", pkg_name, fixed = TRUE)) 
      usethis::use_dev_package(package = pkg_name, remote = pkg, type = "Imports")
    else if (pkg_name %in% suite_pkgs_names)
      usethis::use_package(pkg_name, type = "Depends", min_version = TRUE)
    else
      usethis::use_package(pkg_name, type = "Imports", min_version = TRUE)
  }
}