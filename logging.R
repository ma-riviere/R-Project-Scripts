#=========================#
#### Logging functions ####
#=========================#

# See: https://github.com/r-lib/crayon

log.title <- function(msg) {
  if(suppressWarnings({library(crayon, logical.return = TRUE)})) msg <- crayon::magenta$bold(msg)
  return(cat("\n", msg, "\n"))
}

log.main <- function(msg) {
  if(suppressWarnings({library(crayon, logical.return = TRUE)})) msg <- crayon::blue(msg)
  return(cat("\n", msg, "\n"))
}

log.note <- function(msg) {
  if(suppressWarnings({library(crayon, logical.return = TRUE)})) msg <- crayon::silver$italic(msg)
  return(cat("\n", msg, "\n"))
}

log.warn <- function(msg) {
  if(suppressWarnings({library(crayon, logical.return = TRUE)})) msg <- crayon::yellow(msg)
  return(cat("\n[WARN]", msg, "\n"))
}

log.error <- function(msg) {
  if(suppressWarnings({library(crayon, logical.return = TRUE)})) msg <- crayon::red$bold(msg)
  return(cat("\n[ERROR]", msg, "\n"))
}