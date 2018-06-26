#' Write setup chunk
#'
#' Write setup chunk.
#' @param path Path of working directory.
#' @export

write_setup_chunk <- function(path=NULL){
  if (is.null(path)) path <- getwd()
  writeLines("```{r setup, include=FALSE}")
  writeLines("knitr::opts_chunk$set(echo = FALSE, include = FALSE)")
  r_code <- c('setwd("B:/")', 'source("fcns/config.r")', paste0('setwd("', path, '")'))
  writeLines(r_code)
  writeLines("```")
}
