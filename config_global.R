#=====================#
#### Global Config ####
#=====================#

cat(main("\n[CONFIG] Loading Global Configs ...\n"))

Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = "false")
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "false")
Sys.setenv("_R_USE_PIPEBIND_" = "TRUE")

options(
  scipen = 999L, 
  digits = 4L,
  mc.cores = max(1, parallel::detectCores(logical = TRUE)),
  na.action = "na.omit",
  seed = 256
)

set.seed(getOption("seed"))

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  echo = FALSE,
  message = FALSE,
  cache = FALSE,
  cache.comments = FALSE,
  autodep = FALSE,
  # cache.extra = list(R.version.string, knitr::rand_seed),
  fig.align = "center",
  fig.retina = 3,
  fig.width = 10,
  dpi = 600,
  dev = 'svg',
  dev.args = list(bg = "transparent")
)

knitr::opts_knit$set(
  root.dir = here::here(),
  verbose = FALSE
)

#-----------------------#
#### Package options ####
#-----------------------#

load_global_config <- function() {
  global_config <- tryCatch(
    config::get(file = "global_config.yml"), 
    error = \(e) return(NULL)
  )
  
  if(!is.null(global_config)) cat(note("\n[CONFIG] Global config file found.\n"))
  else cat(warn("\n[CONFIG] No global config file found.\n"))
  
  return(global_config)
}

configure_git <- function() {
  if(Sys.getenv("GITHUB_PAT") != "") {
    cat(note("\n[CONFIG] GITHUB Access Token found: ", Sys.getenv("GITHUB_PAT"), "\n"))
  }
  else if (!is.null(global_config$github_pat) && global_config$github_pat != "") {
    cat(note("\n[CONFIG] GITHUB Access Token found: ", global_config$github_pat, "\n"))
    Sys.setenv(GITHUB_PAT = global_config$github_pat)
  }
  else cat(warn("\n[CONFIG] GITHUB Access Token NOT found - package loading might fail due to Github API's download cap.\n"))
}

configure_packages <- function() {
  
  installed_packages <- get_renv_installed_pkgs()
  
  if ("rstan" %in% installed_packages) rstan::rstan_options(auto_write = TRUE)
  
  if ("loo" %in% installed_packages) options(loo.cores = getOption("mc.cores"))
  
  if ("dtplyr" %in% installed_packages || "tidytable" %in% installed_packages || "data.table" %in% installed_packages) data.table::setDTthreads(getOption("mc.cores"))
  
  if ("furrr" %in% installed_packages) {
    future::plan(multisession, workers = getOption("mc.cores"))
    furrr::furrr_options(seed = getOption("seed"))
  }
  
  if ("afex" %in% installed_packages) {
    # afex::set_sum_contrasts()
    afex::afex_options(
      type = 3,
      method_mixed = "KR",
      include_aov = TRUE,
      factorize = FALSE,
      check_contrasts = FALSE,
      es_aov = "pes", # ges
      correction_aov = "HF",
      emmeans_model  = "multivariate"
    )
  }
  
  if ("emmeans" %in% installed_packages) {
    emmeans::emm_options(
      lmer.df = "kenward-roger",
      opt.digits = 4,
      back.bias.adj = FALSE 
      # don't forget to use bias.adjust = T for mixed models and models with response transforms (e.g. `log(Y) ~ .`)
    )
  }
  
  if ("lme4" %in% installed_packages && "optimx" %in% installed_packages) {
    my.lmer.control.params <<- lme4::lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))
    my.glmer.control.params <<- lme4::glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))
  }
  
}

configure_stan <- function(rebuild = FALSE, openCL = FALSE, version = "2.28.1") {
  
  if("cmdstanr" %in% get_renv_installed_pkgs()) {
    
    ## Init
    
    if (Sys.info()[["sysname"]] == "Windows") cmdstan_install_path <<- normalizePath("D:/Dev/SDK/.cmdstanr/") # TODO: when BUG is fixed, remove .cmdstanr/
    else if (Sys.info()[["sysname"]] == "Linux") cmdstan_install_path <<- normalizePath("/home/mar/.cmdstanr/") # TODO: when BUG is fixed, remove .cmdstanr/
    
    if(!dir.exists(cmdstan_install_path)) dir.create(cmdstan_install_path)
    
    # OLD_HOME <- Sys.getenv("HOME") # BUG: not needed while BUG is not fixed
    # Sys.setenv(HOME = cmdstan_install_path)
    
    cmdstan_file_name <- paste0("cmdstan-", version)
    cmdstan_path <- normalizePath(file.path(cmdstan_install_path, cmdstan_file_name))
    cmdstanr::set_cmdstan_path(cmdstan_path)
    
    Sys.setenv(CMDSTAN = cmdstan_install_path)
    
    if (rebuild) {
      
      cpp_opts <- list(stan_threads = TRUE, "PRECOMPILED_HEADERS" = TRUE, "STAN_CPP_OPTIMS" = TRUE)
      
      ## OpenCL
      
      if (openCL && Sys.info()[["sysname"]] == "Windows") {
        
        CUDA_PATH <- normalizePath("D:/Program Files/CUDA/lib/x64")
        
        cpp_opts <- append(
          cpp_opts,
          c(
            "STAN_OPENCL" = TRUE,
            # "PRECOMPILED_HEADERS" = FALSE,
            "OPENCL_DEVICE_ID" = 0,
            "OPENCL_PLATFORM_ID" = 0,
            paste0("LDFLAGS_OPENCL=-L\"", CUDA_PATH, "\" -lOpenCL") # paste0("LDFLAGS= -L\"", CUDA_PATH, "\" -lOpenCL")
          )
        ) 
      }
      
      ## Install
      
      if (Sys.info()[["sysname"]] == "Windows") {
        # cmdstanr::install_cmdstan(overwrite = TRUE, cpp_options = cpp_opts, version = version, quiet = TRUE)

        cmdstan_archive_url <- glue::glue("https://github.com/stan-dev/cmdstan/releases/download/v{version}/{cmdstan_file_name}.tar.gz")
        cmdstan_archive_path <- paste0(cmdstan_path, ".tar.gz")
        download.file(cmdstan_archive_url, destfile = cmdstan_archive_path, mode = "wb")
        untar(tarfile = cmdstan_archive_path, exdir = cmdstan_install_path)
        cmdstanr::set_cmdstan_path(cmdstan_path) # BUG (2.28.1): Has to be here too
        cmdstanr::cmdstan_make_local(dir = cmdstan_path, cpp_options = cpp_opts)
        cmdstanr::rebuild_cmdstan(dir = cmdstan_path, quiet = TRUE)
        if (file.exists(cmdstan_archive_path)) file.remove(cmdstan_archive_path)
      }
      else if (Sys.info()[["sysname"]] == "Linux") {
        cmdstanr::install_cmdstan(overwrite = TRUE, cpp_options = cpp_opts, version = version, quiet = TRUE)
      }
      
      # Sys.setenv(HOME = OLD_HOME)
    }
    
    CMDSTAN_TBB <- normalizePath(file.path(cmdstan_path, "stan/lib/stan_math/lib/tbb"))
    Sys.setenv("Path" = paste0(Sys.getenv("PATH"), CMDSTAN_TBB))
    
    options(brms.backend = "cmdstanr")
  }
}