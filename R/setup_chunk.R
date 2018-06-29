#' Write setup chunk
#'
#' Write setup chunk.
#' @param path Path of working directory.
#' @export

setup_chunk <- function(path){
  r_code <- c('setwd("B:/")', 'source("fcns/config.r")', paste0('setwd("', path, '")'))
  chunk <- c("```{r setup, include=FALSE}", "knitr::opts_chunk$set(echo = FALSE, include = FALSE)",
             r_code, "```")
  return(chunk)
}
