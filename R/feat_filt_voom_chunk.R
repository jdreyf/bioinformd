#' Feature filter chunk
#'
#' Feature filter chunk.
#'
#' @param row.type Character in filename for features.
#' @details A feature must be present at least 80% of \code{min.npergrp}.
#' @inheritParams bioinfo_rmd_contrasts
#' @export

feat_filt_voom_chunk <- function(grp.var, row.type="row"){
  rows.type <- paste0(row.type, "s")

  r_code <- c(glue::glue("min.npergrp <- min(table(pheno${grp.var}))"),
              "dge <- DGEList(counts=mtrx)",
              "cpm.thresh <- signif(10**7/median(colSums(dge$counts)), digits=2)",
              "feat.ss <- which(rowSums(cpm(dge) > cpm.thresh) >= 0.8*min.npergrp)",
              "dge <- dge[feat.ss,]")

  ffilt.txt <- paste0("Before filtering, there are `r nrow(mtrx)` ", rows.type,
    ". We filter out ", rows.type, " that don't have at least `r cpm.thresh` CPM in `r ceiling(0.8*min.npergrp)` samples.",
    " This threshold is based on the rule-of-thumb that if all samples had the same read depth, we would want to keep genes that have at least 10 counts in multiple samples.",
     " After filtering, there are `r length(feat.ss)` ", rows.type, ".")

  chunk <- c(paste("# Filter", rows.type), "", "```{r feat_filt}", r_code, "```", "", ffilt.txt)
  return(chunk)
}
