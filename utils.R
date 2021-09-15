#===============================#
#### Miscellaneous functions ####
#===============================#

"%ni%" <- Negate("%in%")

### From: https://michaelbarrowman.co.uk/post/getting-a-variable-name-in-a-pipeline/

get_var_name <- function(x) {
  lhs <- get_lhs()
  if(is.null(lhs)) lhs <- rlang::ensym(x)
  return(rlang::as_name(lhs))
}

get_lhs <- function() {
  calls <- sys.calls()
  
  #pull out the function or operator (e.g. the `%>%`)
  call_firsts <- lapply(calls, `[[`, 1) 
  
  #check which ones are equal to the pipe
  pipe_calls <- vapply(call_firsts,identical, logical(1), quote(`%>%`))
  
  #if we have no pipes, then get_lhs() was called incorrectly
  if(all(!pipe_calls)){
    NULL
  } else {
    #Get the most recent pipe, lowest on the 
    pipe_calls <- which(pipe_calls)
    pipe_calls <- pipe_calls[length(pipe_calls)]
    
    #Get the second element of the pipe call
    this_call <- calls[[c(pipe_calls, 2)]]
    
    #We need to dig down into the call to find the original
    while(is.call(this_call) && identical(this_call[[1]], quote(`%>%`))){
      this_call <- this_call[[2]]
    }
    this_call
    
  }
}

### From: https://gist.github.com/alexpghayes/9118cda66375e593343fe28c8d13fdb5
# Upload a data frame to google drive, make it shareable, and copy the shareable link into the clipboard
# See: https://googledrive.tidyverse.org/

# if direct = TRUE, the link can be used immediately to read in the file
# if direct = FALSE, the link takes users to a nice preview of the file instead
get_shareable_link_to_data <- function(data, path, direct = TRUE) {
  readr::write_csv(data, path)
  df <- googledrive::drive_upload(path, path)
  df <- googledrive::drive_share(df, role = "reader", type = "anyone")
  
  if (direct)
    link <- paste0("https://drive.google.com/uc?export=download&id=", df$id)
  else
    link <- googledrive::drive_link(df)
  
  clipr::write_clip(link)
  fs::file_delete(path)
  cat(link)
  invisible(link)
}

hush <- function(output){
  sink("NUL")
  temp <- output
  sink()
  return(temp)
}

save_png <- function(plot, filename = NULL, dpi = 600, width = 8, height = 8, display = TRUE) {
  if(is.null(filename)) filename <- as.list(match.call()[-1])$plot
  filename <- here("fig", paste(filename, ".png", sep = ""))
  ggsave(filename = filename, plot = plot, device = "png", scale = 1, dpi = dpi, width = width, height = height)
  if(display) return(plot)
}

get_current_file_name <- function() {
  rstudioapi::getActiveDocumentContext()$path |> str_split(pattern = "/") |> first() |> last() |> str_split("[.]") |> first() |> first()
}