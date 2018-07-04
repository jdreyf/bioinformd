#' Boxplot chunk
#'
#' Boxplot chunk.
#' @export

boxplot_chunk <- function(){
  bp.txt <- c("## Visualize distributions",
              "We visualize each sample's distribution with a boxplot to ensure the samples are comparable.")

  bp.r <- "boxplot(mtrx, las=3, ylab='log2 expression', main='Expression boxplot')"

  chunk <- c(bp.txt, "", "```{r bp, include=TRUE, fig.height=4}", bp.r, "```")
  return(chunk)
}
