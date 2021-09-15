#==============#
#### Config ####
#==============#

cat(main("\n[CONFIG] Configuring project ...\n\n"))

Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = "false")
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "false")
Sys.setenv("_R_USE_PIPEBIND_" = "TRUE")


load_configs <- function() {
  
  # TODO: tryCatch()
  global_config <- config::get(file = "../../global_config.yml")
  
  # Else try Sys.getenv("GITHUB_PAT")
  
  # TODO: save into Sys.Setenv ???
  if (!is.null(global_config$github_pat) && global_config$github_pat != "") {
    cat(note(paste0("\n[PACKAGES] GITHUB Access Token found: ", global_config$github_pat, "\n\n")))
  } else {
    cat(warn("\n[PACKAGES] GITHUB Access Token NOT found - package loading might fail due to Github API's download cap.\n\n"))
  }
}

if(Sys.info()[["sysname"]] == "Linux" && grepl("WSL2", Sys.info()[["release"]])) {
  cat(note("\n[CONFIG] WSL2 detected, setting up specific Environment Variables.\n\n"))
  # Sys.setenv("OPENBLAS_NUM_THREADS" = "8")
}

options(
  scipen = 999L, 
  digits = 4L,
  # contrasts = c("contr.sum", "contr.poly"), # c("contr.orthonorm", "contr.poly"),
  mc.cores = max(1, parallel::detectCores(logical = TRUE)),
  na.action = "na.omit",
  seed = 256L
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
  verbose = FALSE
  # output.dir = here()
)

alpha <- 0.05

#-----------------#
#### Conflicts ####
#-----------------#

select <- dplyr::select

#-----------------------#
#### Package options ####
#-----------------------#

configure_packages <- function() {
  
  installed_packages <- get_renv_installed_pkgs()
  
  if ("rstan" %in% installed_packages) rstan::rstan_options(auto_write = TRUE)
  
  if ("loo" %in% installed_packages) options(loo.cores = getOption("mc.cores"))
  
  if ("rentrez" %in% installed_packages) {
    ekey <- Sys.getenv("ENTREZ_KEY")
    if (!is.null(ekey)) {
      cat(crayon::blue("\n[OPTS][INFO] Entrez API Key detected and loaded: ", ekey, "\n\n"))
      rentrez::set_entrez_key(ekey)
    }
  }
  
  if ("tidytable" %in% installed_packages || "data.table" %in% installed_packages) data.table::setDTthreads(getOption("mc.cores"))
  
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
      emmeans_model  = "multivariate" # "univariate" (default) --> use aov object || "multivariate" --> use lm  object (or mlm for repeated-measures)
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

configure_stan <- function(rebuild = FALSE, openCL = FALSE, version = "2.26.1") {
  if("cmdstanr" %in% installed_packages) {
    cmdstanr::check_cmdstan_toolchain(fix = TRUE)
    
    sys.name <- Sys.info()
    
    if (Sys.info()[["sysname"]] == "Windows") cmdstan_install_path <<- "D:\\Dev\\SDK\\"
    else if (Sys.info()[["sysname"]] == "Linux") cmdstan_install_path <<- "/home/mar/.cmdstanr/"
    
    cmdstan_version_name <- paste0("cmdstan-", version)
    
    cmdstanr::set_cmdstan_path(path = paste0(cmdstan_install_path, cmdstan_version_name))
    
    if (rebuild) {
      if (openCL) {
        path_to_opencl_lib <- "C:/Program Files/NVIDIA GPU Computing Toolkit/CUDA/v11.3/lib/x64"
        cpp_opts <<- list(
          "CXXFLAGS += -fpermissive", "STAN_OPENCL" = TRUE,
          "stan_threads" = FALSE, "PRECOMPILED_HEADERS" = FALSE, "STAN_CPP_OPTIMS" = TRUE, "STAN_NO_RANGE_CHECKS" = TRUE,
          paste0("LDFLAGS+= -L\"", path_to_opencl_lib, "\" -lOpenCL")
        )
      } else {
        cpp_opts <<- list("stan_threads" = FALSE, "STAN_NO_RANGE_CHECKS" = TRUE, "PRECOMPILED_HEADERS" = TRUE, "STAN_CPP_OPTIMS" = TRUE)
      }
      
      if (Sys.info()[["sysname"]] == "Windows") {
        # cmdstanr::install_cmdstan(dir = cmdstan_install_path, overwrite = T, cpp_options = cpp_opts, version = version)
        
        cmdstan_archive_url <- glue::glue("https://github.com/stan-dev/cmdstan/releases/download/v{version}/{cmdstan_version_name}.tar.gz")
        cmdstan_archive_path <- glue::glue("{cmdstan_install_path}{cmdstan_version_name}.tar.gz")
        download.file(cmdstan_archive_url, destfile = cmdstan_archive_path, mode = "wb")
        # untar(cmdstan_archive_path) # TODO: find working method of unpacking
        
        cmdstanr::cmdstan_make_local(cpp_options = cpp_opts)
        cmdstanr::rebuild_cmdstan(quiet = TRUE)
      }
      else if (Sys.info()[["sysname"]] == "Linux") {
        cmdstanr::install_cmdstan(dir = cmdstan_install_path, overwrite = T, cpp_options = cpp_opts, version = version)
        # cmdstanr::install_cmdstan(overwrite = T, cpp_options = list("STAN_THREADS" = FALSE, "STAN_NO_RANGE_CHECKS" = TRUE, "PRECOMPILED_HEADERS" = TRUE, "STAN_CPP_OPTIMS" = TRUE), version = "2.26.1")
      }
    }
    
    ### Setting env vars:
    cmdstan_home <- normalizePath(cmdstanr::cmdstan_path())
    Sys.setenv(CMDSTAN_HOME = cmdstan_home)
    
    # cmdstan_tbb <- "%CMDSTAN_HOME%\\stan\\lib\\stan_math\\lib\\tbb"
    # Sys.setenv("Path" = paste0(Sys.getenv("PATH"), cmdstan_tbb))
    
    options(brms.backend = "cmdstanr")
    
    # cmdstanr::register_knitr_engine(override = FALSE)
  }
}