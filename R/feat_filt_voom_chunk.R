#' Feature filter chunk
#'
#' Feature filter chunk.
#'
#' @param min.npergrp Minimum sample size per group.
#' @param row.type Character in filename for features.
#' @details A feature must be present at least 80% of \code{min.npergrp}.
#' @export

feat_filt_voom_chunk <- function(min.npergrp, row.type="row"){
  rows.type <- paste0(row.type, "s")

  r_code <- c(paste0("min.npergrp <- ", min.npergrp),
              "dge <- DGEList(counts=mtrx)",
              "feat.ss <- which(rowSums(cpm(dge) > 1) >= 0.8*min.npergrp)",
              "dge <- dge[feat.ss,]")

  ffilt.txt <- paste0("Before filtering, there are `r nrow(mtrx)` ", rows.type,
    ". We filter out ", rows.type, " that don't have at least 1 CPM in `r ceiling(0.8*min.npergrp)` samples.",
     " After filtering, there are `r length(feat.ss)` ", rows.type, ".")

  chunk <- c(paste("## Filter", rows.type), "```{r feat_filt}", r_code, "```", ffilt.txt)
  return(chunk)
}
