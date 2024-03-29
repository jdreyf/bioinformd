#' Limma contrasts chunk
#'
#' Limma contrasts chunk.
#'
#' @param grp.var Variable name in \code{pheno} for group.
#' @param contr.v Named vector of contrasts.
#' @param path Path of RMD.
#' @param proj.nm Name of project to paste into filenames.
#' @param limma.model Model formula for \code{arrayWeights}.
#' @param use_aw Logical indicating if array weights should be used.
#' @param use_trend Logical indicating if \code{limma trend} should be used.
#' @param use_annot Should \code{annot} be used.
#' @param row.type Character in filename for features.
#' @param elst Logical indicating if expression object an EList.
#' @export

limma_contrasts_chunk <- function(grp.var, contr.v, path, proj.nm, limma.model=NULL, use_aw=TRUE, use_trend=FALSE,
                                  use_annot=TRUE, row.type="gene", elst=FALSE){
  if (!is.null(limma.model)) paste0("des <- model.matrix(", limma.model, ")")

  lc.r <- paste0("contr.v <- ", contr.v)

  if (elst){
    lc.r <- c(lc.r, paste0("mtt <- limma_contrasts(elst, grp=pheno[,'", grp.var, "'], contrast.v=contr.v"))
  } else {
    lc.r <- c(lc.r, paste0("mtt <- limma_contrasts(mtrx, grp=pheno[,'", grp.var, "'], contrast.v=contr.v, trend=", use_trend))
  }

  if (!is.null(limma.model)) lc.r[2] <- paste0(lc.r[2], ", design=des")
  if (use_aw) lc.r[2] <- paste0(lc.r[2], ", weights=aw")
  lc.r[2] <- paste0(lc.r[2], ")")
  if (use_annot){
    lc.r <- c(lc.r, "mtt.df <- data.frame(signif(mtt, 3), annot[rownames(mtt),])")
  } else{
    lc.r <- c(lc.r, "mtt.df <- data.frame(signif(mtt, 3))")
  }
  lc.r <- c(lc.r, paste0("write.csv(mtt.df, '", proj.nm, "_", row.type, "_stats.csv')"))

  lc.txt <- c(paste0("We test differential abundance of each row of the expression matrix using two-tailed unpaired moderated t-tests from ",
                     "the linear regression modeling R package limma [@ritchie_2015].",
                     "The resulting CSV file is at ", rmd_links(filenames = paste0(proj.nm, "_", row.type, '_stats.csv'),
                      path = path), ". It contains the average unweighted log2 expression of each row in each group, p-values, ",
                     "FDRs, log fold-changes, and fold-changes for each comparison, and ", row.type, " annotation."))

  chunk <- c("# Test differential abundance", "", "```{r lc}", lc.r, "```", "", lc.txt)
  return(chunk)
}
