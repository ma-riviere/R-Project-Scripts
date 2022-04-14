#======================#
#### Global Configs ####
#======================#

log.title("[CONFIG] Loading Global Configs ...")

Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = "false")
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "false")
Sys.setenv("_R_USE_PIPEBIND_" = "TRUE")

options(
  scipen = 999L, 
  digits = 4L,
  mc.cores = max(1L, parallel::detectCores(logical = TRUE)),
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
  # fig.width = 10,
  dpi = 600,
  dev = 'png' # svg
  # dev.args = list(bg = "transparent")
)

#---------------#
#### Masking ####
#---------------#

get <- base::get

#-----------------------#
#### Package options ####
#-----------------------#

load_global_config <- function() {
  global_config <- tryCatch(
    config::get(file = "global_config.yml"), 
    error = \(e) return(NULL)
  )
  
  if(!is.null(global_config)) log.note("[CONFIG] Global config file found.")
  else log.warn("[CONFIG] No global config file found.")
  
  return(global_config)
}

configure_git <- function() {
  if(Sys.getenv("GITHUB_PAT") != "") {
    log.note("[CONFIG] GITHUB Access Token found: ", Sys.getenv("GITHUB_PAT"))
  }
  else if (!is.null(global_config$github_pat) && global_config$github_pat != "") {
    log.note("[CONFIG] GITHUB Access Token found: ", global_config$github_pat)
    Sys.setenv(GITHUB_PAT = global_config$github_pat)
  }
  else log.warn("[CONFIG] GITHUB Access Token NOT found - package loading might fail due to Github API's download cap.")
}

configure_packages <- function() {
  
  installed_packages <- get_renv_installed_pkgs()

  if ("rlang" %in% installed_packages && utils::compareVersion(utils::packageVersion("rlang") |> as.character(), "1.0.0") >= 0) {
    log.note("[CONFIG] Activating `rlang` new global trace")
    if (purrr::is_empty(globalCallingHandlers())) {
      log.note("[CONFIG] Activating `rlang` new global trace")
      rlang::global_entrace()
    }
  }
  
  if ("ggplot2" %in% installed_packages) {
    
    invis_custom <<- ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        ## Legend
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    custom_light <<- invis_custom + 
      ggplot2::theme(
        panel.border = element_rect(fill = NA, colour = "black"),
        ## Titles
        plot.title = ggtext::element_markdown(size = 12, face = "bold"),
        plot.subtitle = ggtext::element_markdown(size = 11, face = "italic"),
        ## Legend
        legend.title = ggtext::element_markdown(face = "bold"),
        ## Facets
        strip.background = element_rect(fill = "#ffed75"),
        strip.text = element_text(size = 10, face = "bold"),
        ## Axes
        axis.title.x = ggtext::element_markdown(face = "bold", hjust = 0.5),
        axis.title.y = ggtext::element_markdown(face = "bold", hjust = 0.5),
        axis.text = element_text(color = "black"),
        text = element_text(color = "black")
      )
    
    ggplot2::theme_set(custom_light)
  }
  
  # if ("rstan" %in% installed_packages) rstan::rstan_options(auto_write = TRUE)
  
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
      correction_aov = "HF"
      # emmeans_model  = "univariate"
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

  if ("glmmTMB" %in% installed_packages) {
    options(glmmTMB.cores = max(1L, parallel::detectCores(logical = FALSE)))
  }
}

configure_stan <- function(version = NULL, rebuild = FALSE, openCL = FALSE, BLAS = NULL) {
  
  if("cmdstanr" %in% get_renv_installed_pkgs()) {
    
    ## Initialization
    
    log.note("[CONFIG] Setting up CmdStan ...")
    
    if(is.null(version)) version <- gh::gh("GET /repos/stan-dev/cmdstan/releases/latest")[["tag_name"]] |> substring(2)
    log.note("[CONFIG] Using CmdStan version: ", version)
    
    ### INFO: If env.var "CMDSTAN" exists, then its value will be automatically set as the default path to CmdStan for the R session
    
    if (Sys.info()[["sysname"]] == "Windows") cmdstan_root <<- normalizePath("D:/Dev/SDK/")
    else if (Sys.info()[["sysname"]] == "Linux") cmdstan_root <<- normalizePath("/home/mar/Dev/SDK/")
    
    cmdstan_dir <- ".cmdstan"
    cmdstan_version <- paste0("cmdstan-", version)
    cmdstan_install_path <- normalizePath(file.path(cmdstan_root, cmdstan_dir))
    
    if(!dir.exists(cmdstan_install_path)) dir.create(cmdstan_install_path)
    
    cmdstan_path <- normalizePath(file.path(cmdstan_install_path, cmdstan_version))
    
    ## Rebuilding CmdStan install
    if (rebuild) {
      
      ### General params
      
      #### Changing default installation location (i.e. "HOME") to provided path
      OLD_HOME <- Sys.getenv("HOME")
      Sys.setenv(HOME = cmdstan_root)
      
      cpp_opts <- list(
        STAN_THREADS = TRUE, PRECOMPILED_HEADERS = TRUE, STAN_CPP_OPTIMS = TRUE,
        "CXXFLAGS += -O3 -march=native -mtune=native" # TODO: if issues on Windows -> CXXFLAGS += -march=native
      )

      ### BLAS params
      if (!is.null(BLAS)) {
        
        if(BLAS == "MKL") {
          MKLROOT <- "//usr/include/mkl"

          cpp_opts_mkl <- list(
            glue("CXXFLAGS += -DEIGEN_USE_MKL_ALL -I${MKLROOT}"),
            "LDLIBS += -lmkl_intel_lp64 -lmkl_sequential -lmkl_core" # TODO: use parallel threads instead of sequential ?
          )

          cpp_opts <- append(cpp_opts, cpp_opts_mkl)
        }
        
        if(BLAS == "OB") {
          cpp_opts_blas <- list(
            "CXXFLAGS += -DEIGEN_USE_BLAS -DEIGEN_USE_LAPACKE", 
            "LDLIBS += -lblas -llapack -llapacke"
          )
          
          cpp_opts <- append(cpp_opts, cpp_opts_blas)
        }
      }
      
      ### OpenCL params
      if (openCL) {
        
        cpp_opts_cl <- c(STAN_OPENCL = TRUE, OPENCL_DEVICE_ID = 0, OPENCL_PLATFORM_ID = 0)
        
        if(Sys.info()[["sysname"]] == "Linux") cpp_opts <- append(cpp_opts, cpp_opts_cl)
        
        if(Sys.info()[["sysname"]] == "Windows") {
          if(Sys.getenv("CUDA_PATH") != "") {
            
            CUDA_PATH <- normalizePath(file.path(Sys.getenv("CUDA_PATH"), "/lib/x64"))
            
            if(dir.exists(CUDA_PATH)) {
              log.note("[CONFIG] Found existing CUDA_PATH: ", CUDA_PATH)
              cpp_opts_cl <- append(cpp_opts_cl, paste0("LDFLAGS_OPENCL=-L\"", CUDA_PATH, "\" -lOpenCL"))
              cpp_opts <- append(cpp_opts, cpp_opts_cl)
            }
            else log.warn("[CONFIG] The specified CUDA path does not exist.")
          } 
          else {
            log.warn("[CONFIG] No CUDA_PATH specified in the environment variables.")
          }
        }
      }
      
      ### Installation
      
      if (Sys.info()[["sysname"]] == "Windows") {
        # cmdstanr::install_cmdstan(overwrite = TRUE, cpp_options = cpp_opts, version = version, quiet = TRUE)
        
        cmdstan_archive_name <- paste0(cmdstan_version, ".tar.gz")
        cmdstan_archive_url <- glue::glue("https://github.com/stan-dev/cmdstan/releases/download/v{version}/{cmdstan_archive_name}")
        
        download.file(cmdstan_archive_url, destfile = cmdstan_archive_name, mode = "wb")
        untar(tarfile = cmdstan_archive_name, exdir = cmdstan_install_path)
        cmdstanr::set_cmdstan_path(cmdstan_path) # FIXME (2.28.1): Has to be here too ???
        cmdstanr::cmdstan_make_local(dir = cmdstan_path, cpp_options = cpp_opts, append = FALSE)
        cmdstanr::rebuild_cmdstan(dir = cmdstan_path, quiet = TRUE)
        if (file.exists(cmdstan_archive_name)) file.remove(cmdstan_archive_name)
      }
      else if (Sys.info()[["sysname"]] == "Linux") {
        cmdstanr::install_cmdstan(overwrite = TRUE, cpp_options = cpp_opts, version = version, quiet = TRUE)
      }
      
      Sys.setenv(HOME = OLD_HOME)
      
    } else { ## No rebuild, only configure
      cmdstanr::set_cmdstan_path(cmdstan_path)
    }
    
    if (Sys.info()[["sysname"]] == "Windows") {
      CMDSTAN_TBB <- normalizePath(file.path(cmdstan_path, "stan/lib/stan_math/lib/tbb"))
      Sys.setenv("Path" = paste0(Sys.getenv("PATH"), CMDSTAN_TBB))
    }

    Sys.setenv("OPENBLAS_NUM_THREADS" = 1)
    
    options(brms.backend = "cmdstanr")
  }
}