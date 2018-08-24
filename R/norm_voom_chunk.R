#' Normalization chunk
#'
#' Normalization chunk.
#'
#' @param proj.nm Name of project.
#' @param voom.model Model for design matrix.
#' @param path Path of RMD.
#' @export

norm_voom_chunk <- function(proj.nm, voom.model, path){
  norm.filename <- paste0(proj.nm, "_logCPM.csv")

  r_code <- c(paste0("voom.des <- model.matrix(", voom.model, ", data=pheno)"),
              "dge <- calcNormFactors(dge)",
              "elst <- voom(dge, voom.des, plot=TRUE)",
              "barplot(dge$samples$norm.factors, names.arg=colnames(dge), las = 2)",
              paste0("write.csv(elst$E, '", norm.filename, "')"))

  norm.txt <- c("## Normalize",
                paste0("We normalize samples using the TMM method [@tmm]. The normalization factors range from ",
                       "`r signif(min(dge$samples$norm.factors), 2)` to `r signif(max(dge$samples$norm.factors), 2)`. ",
                       "We use the normalization factors to calculate log2 counts per million (logCPM), and to estimate ",
                       "precision weights given the mean-variance trend, which allows for linear modeling [@voom]. ",
                       "The normalized matrix of logCPM is at ", rmd_links(norm.filename, path=path), "."))

  chunk <- c("```{r norm, include=TRUE,fig.height=4}", r_code, "```", norm.tx)
  return(chunk)
}
