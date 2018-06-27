#' Write data description text
#'
#' Write data description text.
#'
#' @param data.type Description of biomaterial and condition.
#' @param input.files Character vector of input files.
#' @export

data_txt <- function(data.type="Gene expression", input.files=NULL){
  dt <- c("## Data", paste(data.type, "from", paste(rmd_links(input.files), collapse = " and ")))
  return(dt)
}
