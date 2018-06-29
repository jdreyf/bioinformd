#' Feature filter chunk
#'
#' Feature filter chunk.
#'
#' @param min.npergrp Minimum sample size per group.
#' @details A feature must be present at least 80% of \code{min.npergrp}.
#' @export

feat_filt_chunk <- function(min.npergrp){
  r_code <- c(paste0("min.npergrp <- ", min.npergrp),
              "feat.ss <- which(rowSums(!is.na(mtrx.na)) >= 0.8*min.npergrp)",
              "mtrx <- mtrx[feat.ss,,drop=FALSE]")

  ffilt.txt <- paste("Before filtering, there are `r nrow(mtrx.na)` rows.",
    "We filter out rows that don't have at least `r ceiling(0.8*min.npergrp)` non-NA values.",
     "After filtering, there are `r length(feat.ss)` rows.")

  chunk <- c("## Filter rows", "```{r feat_filt}", r_code, "```", ffilt.txt)
  return(chunk)
}
