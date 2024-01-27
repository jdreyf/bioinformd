#' Sample weights chunk
#'
#' Sample weights chunk.
#' @param aw.model Model formula for \code{arrayWeights}.
#' @param elst Logical indicating if expression object an EList.
#' @export

sample_weights_chunk <- function(aw.model=NULL, elst=FALSE){
  #arrayWeightsSimple yielded extreme values on random data, so using arrayWeights
  if (elst){
    if (!is.null(aw.model)){
      sw.r <- c(paste0("aw.des <- model.matrix(", aw.model, ", data=pheno)"),
                "aw <- arrayWeights(elst$E, design=aw.des)")
    } else {
      sw.r <- "aw <- arrayWeights(elst$E)"
    }
  } else {
    if (!is.null(aw.model)){
      sw.r <- c(paste0("aw.des <- model.matrix(", aw.model, ", data=pheno)"),
                "aw <- arrayWeights(mtrx, design=aw.des)")
    } else {
      sw.r <- "aw <- arrayWeights(mtrx)"
    }
  }

  sw.r <- c(sw.r, "names(aw) <- colnames(mtrx)")

  sw.txt <- c(paste("We unbiasedly estimate empirical sample quality weights [@ritchie_2006] in the R package limma.",
                    "Weights here vary from `r signif(min(aw), 2)` to `r signif(max(aw), 2)`.",
                    "If all samples were weighted equally, they would all have weight = 1."))

  sw.r2 <- c("barplot(aw, las=3, main='Sample quality weights')", "abline(h=1, lty=2)")

  chunk <- c("# Estimate sample quality weights", "```{r aw}", sw.r, "```", "",
             "```{r aw_bar, include=TRUE, fig.height=4}", sw.r2, "```", "", sw.txt)
  return(chunk)
}
