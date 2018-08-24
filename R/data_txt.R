#' Write data description text
#'
#' Write data description text.
#'
#' @param input.files Character vector of input files.
#' @param path The path to the input files.
#' @export

data_txt <- function(input.files=NULL, path){
  dt <- c("## Data", paste0("Data from ", paste(rmd_links(input.files, path=path), collapse = " and "), "."))
  return(dt)
}
