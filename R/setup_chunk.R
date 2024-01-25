#' Write setup chunk
#'
#' Write setup chunk.
#'
#' @param path Local path of working directory
#' @param link2wd Boolean; should links be to working directory on Joslin network? Otherwise, links are to local folder
#' @export

setup_chunk <- function(path, link2wd = TRUE){
  wd.path <- ifelse(link2wd, paste0("J:/cores/bioinformatics/", path), ".")
  r_code <- c('setwd("B:/")', 'source("fcns/config.r")', paste0('setwd("', path, '")'), paste0('wd <- "', wd.path, '"'))
  chunk <- c("```{r setup, include=FALSE}", "knitr::opts_chunk$set(echo = FALSE, include = FALSE)",
             r_code, "```")
  return(chunk)
}
