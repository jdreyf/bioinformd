#' R Session code chunk
#'
#' R Session using pander code chunk.
#'
#' @export

session_chunk <- function(){
  session.r <- "pander::pander(sessionInfo())"
  chunk <- c("# R session", "",  "```{r session.info, include=TRUE}", session.r, "```")
  return(chunk)
}
