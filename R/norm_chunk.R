#' Normalization chunk
#'
#' Normalization chunk.
#'
#' @param method Character string with method.
#' @export

norm_chunk <- function(input.files){
  norm.filename <- paste0(sub(pattern="\\....$", "", input.files[1]), "_norm.csv")

  r_code <- c("cmed <- apply(mtrx, MARGIN=2, FUN=median)",
              "mtrx <- scale(mtrx, center=FALSE, scale=cmed/median(mtrx))",
              paste0("write.csv(mtrx, '", norm.filename, "')"))

  norm.txt <- c("## Normalize", paste0("We normalize samples to have the same median. The normalized matrix is at ",
              rmd_links(norm.filename), ". After normalization, each column's median is `r signif(median(mtrx), 2)`."))

  chunk <- c(norm.txt, "```{r norm}", r_code, "```")
  return(chunk)
}
