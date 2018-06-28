#' PCA chunk
#'
#' PCA chunk.
#' @param grp.var Variable name in \code{pheno} for group.
#' @param covars Variable name in \code{pheno} for covariates.

pca_chunk <- function(grp.var, covars=NULL){
  pca.r <- c(paste0("ezpca(mtrx, pheno, color = '", grp.var, "', labels = TRUE, name = NA)"),
             paste0("ezpca(mtrx, pheno, color = '", grp.var, "', name = 'pca')"))

  pca.txt <- c("## Principal Components Analysis (PCA)",
               paste("We visualize the clustering of samples in 2D using the first two principal components.",
                     "These two axes capture the maximum amount of variation in the data. The proportion of variation",
                     "captured by each axis is shown in parentheses. A PCA with labeled samples is shown here.",
                     "A PCA without labels is at", rmd_links(filenames = "pca.png"), "."))

  if (!is.null(covars)){
    pca.r <- c(pca.r, paste0("multi_covar_pca(mtrx, pheno, grp.var='", grp.var, "')"))
    pca.txt <- c(pca.txt, paste0("A PCA with each covariate is at ", rmd_links("covar_pca.pdf"), "."))
  }

  chunk <- c(pca.txt, "", "```{r pca, include=TRUE, fig.height=5}", pca.r, "```")
  return(chunk)
}
