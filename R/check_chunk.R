#' Check code chunk
#'
#' Check code chunk.
#'
#' @export

check_chunk <- function(){
  check.r <- c("stopifnot(rownames(pheno) == colnames(mtrx))",
               "stopifnot(rownames(mtrx) %in% rownames(annot))")
  chunk <- c("```{r check}", check.r, "```")
  return(chunk)
}
