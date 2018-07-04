#' Meanvar trend chunk
#'
#' Meanvar trend chunk.
#' @param voom.model Model formula for \code{arrayWeights}.
#' @param use_trend Logical indicating if \code{limma trend} should be used.
#' @param row.type Character in filename for features.
#' @export

meanvar_trend_chunk <- function(voom.model, use_trend=FALSE, row.type="row"){
  mvt.r <- c(paste0("voom.des <- model.matrix(", voom.model, ", data=pheno)"),
             "v <- voom(2^mtrx, design=voom.des, plot = TRUE)")

  mvt.txt <- c("## Assess mean-variance trend",
              paste0("We assess if there is a trend between a", row.type, "'s average abundance and its variance."))

  if (use_trend){
    mvt.txt <- c(mvt.txt, "We see a trend, so we account for it in variance estimation using limma-trend [@voom].")
  } else {
    mvt.txt <- c(mvt.txt, "We do not see a robust enough trend that is worth accounting for in variance estimation.")
  }

  chunk <- c(mvt.txt, "", "```{r mvt, include=TRUE, fig.height=4}", mvt.r, "```")
  return(chunk)
}
