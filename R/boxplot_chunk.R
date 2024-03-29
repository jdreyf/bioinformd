#' Boxplot chunk
#'
#' Boxplot chunk.
#' @param elist Logical indicating if expression object an EList.
#' @export

boxplot_chunk <- function(elist=FALSE){
  bp.header <- "# Visualize distributions"
  bp.txt <- c("We visualize each sample's distribution with a boxplot to ensure the samples are comparable.")

  if (elist){
    bp.r <- "boxplot(elst$E, las=3, ylab='log2 CPM', main='Expression boxplot')"
  } else {
    bp.r <- "boxplot(mtrx, las=3, ylab='log2 expression', main='Expression boxplot')"
  }

  chunk <- c(bp.header, "", "```{r bp, include=TRUE, fig.height=4}", bp.r, "```", "", bp.txt)
  return(chunk)
}
