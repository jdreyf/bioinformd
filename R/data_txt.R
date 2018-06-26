#' Write data description text
#'
#' Write data description text.
#'
#' @param data.type Description of biomaterial and condition.
#' @param input.files Character vector of input files.
#' @export

data_txt <- function(data.type="Gene expression", input.files=NULL){
  dt <- c("## Data", paste0(data.type, "."))
  rl <- rmd_links(input.files)
  return(c(dt, rl))
}
