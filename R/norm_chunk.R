#' Normalization chunk
#'
#' Normalization chunk.
#'
#' @param proj.nm Project name.
#' @param path Path of RMD.
#' @export

norm_chunk <- function(proj.nm, path){
  norm.filename <- paste0(proj.nm, "_norm.csv")

  r_code <- glue::glue('
  cmed <- apply(mtrx, MARGIN=2, FUN=median)
  # make sure data are logged
  stopifnot(mtrx < 50)
  mtrx <- scale(mtrx, center = cmed - median(mtrx), scale=FALSE)
  stopifnot(apply(mtrx, MARGIN=2, FUN=median) == median(mtrx))
  write.csv(mtrx, "{norm.filename}")
                       ')

  norm.txt <- c(paste0("We normalize samples to have the same median. The normalized matrix is at ",
              rmd_links(norm.filename, path=path),
              ". After normalization, each sample's median is `r signif(median(mtrx), 2)`."))

  chunk <- c("# Normalize", "", "```{r norm}", r_code, "```", "", norm.txt)
  return(chunk)
}
