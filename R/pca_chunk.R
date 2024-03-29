#' PCA chunk
#'
#' PCA chunk.
#' @param grp.var Variable name in \code{pheno} for group.
#' @param proj.nm Name of project to paste into filenames.
#' @param covars Variable name in \code{pheno} for covariates.
#' @param elst Logical indicating if expression object an EList.
#' @export

pca_chunk <- function(grp.var, proj.nm, covars=NULL, elst=FALSE){
  if (elst){
    pca.r <- c(paste0("ezpca(elst$E, pheno, color = '", grp.var, "', labels = TRUE, name = NA)"),
               paste0("ezpca(elst$E, pheno, color = '", grp.var, "', name = '", proj.nm, "_pca')"))
  } else {
    pca.r <- c(paste0("ezpca(mtrx, pheno, color = '", grp.var, "', labels = TRUE, name = NA)"),
               paste0("ezpca(mtrx, pheno, color = '", grp.var, "', name = '", proj.nm, "_pca')"))
  }

  pca.header <- "# Principal Components Analysis (PCA)"
  pca.txt <- c(paste0("We visualize the clustering of samples in 2D using the first two principal components. ",
                     "These two axes capture the maximum amount of variation in the data. The proportion of variation ",
                     "captured by each axis is shown in parentheses. A PCA with labeled samples is shown here. ",
                     "A PCA without labels is at ", rmd_links(paste0(proj.nm, "_pca.pdf")), "."))

  if (!is.null(covars)){
    pca.r <- c(pca.r, paste0("multi_covar_pca(mtrx, pheno, name='", proj.nm, "_covar_pca', grp.var='", grp.var, "')"))
    pca.txt[2] <- paste0(pca.txt[2], " A PCA with each covariate is at ", rmd_links(paste0(proj.nm, "_covar_pca.pdf")), ".")
  }

  chunk <- c(pca.header, "", "```{r pca, include=TRUE, fig.height=5}", pca.r, "```", "", pca.txt)
  return(chunk)
}
