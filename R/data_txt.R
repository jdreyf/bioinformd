#' Write data description text
#'
#' Write data description text.
#'
#' @param data.type Description of biomaterial and condition.
#' @param input.files Character vector of input files.
#' @export

data_txt <- function(data.type="Gene expression", input.files=NULL, path){
  dt <- c("## Data", paste0(data.type, " from ", paste(rmd_links(input.files, path=path), collapse = " and "), "."))
  return(dt)
}
