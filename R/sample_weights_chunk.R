#' Sample weights chunk
#'
#' Sample weights chunk.
#' @param aw.model Model formula for \code{arrayWeights}.
#' @export

sample_weights_chunk <- function(aw.model=NULL){
  if (!is.null(aw.model)){
    sw.r <- c(paste0("aw.des <- model.matrix(", aw.model, ", data=pheno)"),
                "aw <- arrayWeightsSimple(mtrx, design=aw.des)")
  } else {
    sw.r <- "aw <- arrayWeightsSimple(mtrx)"
  }
  sw.r <- c(sw.r, "names(aw) <- colnames(mtrx)")

  sw.txt <- c("## Estimate sample quality weights",
              paste("We unbiasedly estimate emprical sample quality weights in the R package limma [@arrayweights].",
                    "Weights here vary from `r signif(min(aw), 2)` to `r signif(max(aw), 2)`."))

  sw.r2 <- c("barplot(aw, las=3, main='Sample quality weights')", "abline(h=1, lty=2)")

  chunk <- c(sw.txt[1], "```{r aw}", sw.r, "```", sw.txt[-1], "", "```{r aw_bar, include=TRUE, fig.height=4}",
             sw.r2, "```")
  return(chunk)
}
