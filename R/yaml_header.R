#' Write YAML header
#'
#' Write YAML header for project.
#' @param yaml.title Title of project
#' @export

#need double quotes on outside, single on inside
yaml_header <- function(yaml.title){
  #need to use double quotes for yaml
  yaml <- c(paste0('title: "', yaml.title, '"'), paste('date: "`r Sys.time()`"'), "output: word_document",
            "bibliography: B:/annotations/bib/bioinfo.bib")
  yh <- c("---", yaml, "---")
  return(yh)
}
