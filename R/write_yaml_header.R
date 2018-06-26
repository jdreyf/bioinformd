#' Write YAML header
#'
#' Write YAML header for project.
#' @param title Title of project
#' @export

#need double quotes on outside, single on inside
write_yaml_header <- function(title){
  writeLines("---")
  yaml <- c(paste0("title: '", title, "'"), paste("date: '`r Sys.time()`'"), "output: word_document")
  writeLines(yaml)
  writeLines(c("---","")) #add extra newline
}
