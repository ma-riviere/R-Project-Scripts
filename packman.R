#=======================#
#### Package Manager ####
#=======================#

options(
  pkgType = ifelse(Sys.info()[["sysname"]] == "Windows", "both", "source"),
  Ncpus = max(1, parallel::detectCores(logical = TRUE) - 1),
  # install.packages.check.source = "no",
  verbose = FALSE
)

if (any(c("rstan", "cmdstanr") %in% strsplit(proj_pkg, "/"))) {
  options(repos = c(STAN = "https://mc-stan.org/r-packages/", CRAN = "https://cloud.r-project.org/"))
}

Sys.setenv(MAKEFLAGS = paste("-j", getOption("Ncpus"), sep = ""))

suite_pkgs_names <- c("tidyverse", "tidymodels", "easystats")

#---------------------#
#### Main function ####
#---------------------#

update_packages <- function(project_pkgs, update = FALSE, clean = TRUE) {
  
  if(clean) {
    cat(note("\n[PACKAGES] Cleaning illegal project packages ...\n\n"))
    renv::clean(prompt = FALSE)
    
    # TODO: make separate function
    # unlink(dir(path = 'C:/Users/.../AppData/Local/Temp/', pattern = "Rtmp.*", recursive = FALSE, full.names = TRUE), recursive = T)
    # clean_unlisted()
  }
  
  if(update) {

    cat(note("\n[PACKAGES] Installing project packages ...\n\n"))
    install_packages(project_pkgs)
    
    cat(note("\n[PACKAGES] Loading project packages ...\n\n"))
    load_packages(c(common_pkgs, project_pkgs))
    
    cat(note("\n[PACKAGES] Configuring project packages ...\n\n"))
    configure_packages()
    
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
    add_packages_to_description(c(common_pkgs, project_pkgs))
    
    ## Updating renv.lock
    renv::snapshot(type = "explicit", prompt = FALSE)
    
  } else {
    renv::restore(prompt = FALSE)
    load_packages(c(common_pkgs, project_pkgs))
  }
}

#------------------------#
#### Helper functions ####
#------------------------#

get_pkg_name <- function(pkg) {
  pkg_name <- pkg
  if (grepl("/", pkg, fixed = TRUE)) {
    pkg_path <- strsplit(pkg, "/", fixed = TRUE)[[1]]
    pkg_name <- pkg_path[length(pkg_path)]
  }
  if (grepl("@", pkg_name, fixed = TRUE)) {
    pkg_path <- strsplit(pkg, "@", fixed = TRUE)[[1]]
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
  
  if (is_installed(pkg_name)) if (packageVersion(pkg_name) == get_pkg_version(pkg)) return(FALSE)
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

# clean_unlisted <- function(pkgs = c(base_pkg, proj_pkg)) {
#   print("[DEP][INFO] Finding unused dependencies to clean")
#   needed <- c()
#   pkgs <- c(pkgs, c("base", "class", "compiler"))
#   for(pkg in pkgs) {
#     pkg <- get_pkg_name(pkg)
#     deps <- tools::package_dependencies(pkg, recursive = T, which = "all") #, which = c("Depends", "Imports")
#     if(!is.null(deps[[pkg]]))
#       needed <- unique(c(needed, deps[[pkg]], pkg))
#     else
#       needed <- unique(c(needed, pkg))
#   }
#   installed <- as.data.frame(utils::installed.packages(lib.loc = renv::paths$library(), noCache = T))$Package
#   to_remove <- installed[installed %ni% needed]
#   renv::remove(to_remove)
# }
# 
# remove_dependencies <- function(pkg, recursive = FALSE) {
#   d <- package_dependencies(utils::installed.packages(lib.loc = renv::paths$library(), noCache = T), recursive = recursive) # (, installed...)
#   depends <- if (!is.null(d[[pkg]])) d[[pkg]] else character()
#   needed <- unique(unlist(d[names(d) %ni% c(pkg, depends)]))
#   toRemove <- depends[depends %ni% needed]
#   if (length(toRemove)) {
#     renv::remove(toRemove)
#   }
# }

## TODO (if DESCRIPTION not detected by Shinyapps): loop to write library() calls in dependencies.R