#=============================#
#### CmdStan configuration ####
#=============================#

configure_stan <- function(version = NULL, rebuild = FALSE, openCL = FALSE, BLAS = NULL) {
  
  if(is_installed("cmdstanr")) {
    
    ## Initialization
    
    log.main("[CONFIG] Setting up CmdStan ...")
    
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
        cmdstanr::check_cmdstan_toolchain(fix = TRUE)
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