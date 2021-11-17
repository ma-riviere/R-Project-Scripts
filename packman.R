#========================#
#### Packages Manager ####
#========================#

options(
  repos = project_repos,
  pkgType = ifelse(Sys.info()[["sysname"]] == "Windows", "both", "source"),
  Ncpus = max(1, parallel::detectCores(logical = TRUE) - 1),
  # install.packages.check.source = "no",
  verbose = FALSE
)

Sys.setenv(MAKEFLAGS = paste0("-j", getOption("Ncpus")))

suite_pkgs_names <- c("tidyverse", "tidymodels", "easystats")

#---------------------#
#### Main function ####
#---------------------#

init_project_packages <- function(update = FALSE, clean = TRUE) {
    
  if(update) {

    if(clean) {
      log.main("[PACKAGES] Cleaning illegal project packages ...")
      renv::clean(prompt = FALSE)
    }
    
    # log.title("[PACKAGES] Updating submodules ...")
    # update_submodules()
    
    log.title("[PACKAGES] Configuring GITHUB access ...")
    configure_git()

    
    
    log.title("[PACKAGES] Installing project packages ...")
    install_packages(project_pkgs)
    
    log.main("[PACKAGES] Loading project packages ...")
    load_packages(project_pkgs)
    
    log.main("[PACKAGES] Indexing project packages ...\n")
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
    log.title("[PACKAGES] Restoring project packages ...")
    renv::restore(prompt = FALSE)
    load_packages(project_pkgs)
  }
  
  log.title("[PACKAGES] Configuring project's packages ...")
  configure_packages()
}

init_base_packages <- function() {
  install_packages(base_pkgs)
  load_packages(base_pkgs)
  
  log.title("[PACKAGES] Base packages installed.")
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
      if (should_install(pkg)) {
        tryCatch({
          renv::install(packages = pkg, prompt = FALSE, build_vignettes = FALSE)
        }, error = function(e) {
          log.warn("[PACKAGES] Error installing package", pkg, "from source. Attempting binary install ...\n")
          renv::install(packages = pkg, prompt = FALSE, build_vignettes = FALSE, type = "binary")
        })
      }
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