#' Normalization chunk
#'
#' Normalization chunk.
#'
#' @param input.files Character vector of input files. Should be data.matrix, pheno, then optionally annot.
#' @param path Path of RMD.
#' @export

norm_chunk <- function(proj.nm, path){
  norm.filename <- paste0(proj.nm, "_norm.csv")

  r_code <- c("cmed <- apply(mtrx, MARGIN=2, FUN=median)",
              "mtrx <- scale(mtrx, center=FALSE, scale=cmed/median(mtrx))",
              paste0("write.csv(mtrx, '", norm.filename, "')"))

  norm.txt <- c("## Normalize", paste0("We normalize samples to have the same median. The normalized matrix is at ",
              rmd_links(norm.filename, path=path),
              ". After normalization, each sample's median is `r signif(median(mtrx), 2)`."))

  chunk <- c(norm.txt, "```{r norm}", r_code, "```")
  return(chunk)
}
