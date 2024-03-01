#' Write YAML header
#'
#' Write YAML header for project.
#' @param yaml.title Title of project
#' @export

#need double quotes on outside, single on inside
yaml_header <- function(yaml.title){
  #need to use double quotes for yaml
  yaml <- c(paste0('title: "', yaml.title, '"'),
            paste('date: "`r Sys.time()`"'),
            glue::glue("
output:
  html_document:
    code_folding: hide
    df_print: paged
    highlights: pygments
    number_sections: true
    self_contained: true
    theme: default
    toc: true
    toc_float:
     collapsed: true
     smooth_scroll: true"),
            "bibliography: B:/annotations/bib/bioinfo.bib")
  yh <- c("---", yaml, "---")
  return(yh)
}
