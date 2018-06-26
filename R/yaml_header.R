#' Write YAML header
#'
#' Write YAML header for project.
#' @param title Title of project
#' @export

#need double quotes on outside, single on inside
yaml_header <- function(title){
  yaml <- c(paste0("title: '", title, "'"), paste("date: '`r Sys.time()`'"), "output: word_document")
  yh <- c("---", yaml, "---")
  return(yh)
}
