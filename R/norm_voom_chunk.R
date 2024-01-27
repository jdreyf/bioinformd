#' Normalization chunk
#'
#' Normalization chunk.
#'
#' @param proj.nm Name of project.
#' @param voom.model Model for design matrix.
#' @param path Path of RMD.
#' @param use_aw Logical indicating if array weights should be used.
#' @export

norm_voom_chunk <- function(proj.nm, voom.model, path, use_aw){
  norm.filename <- paste0(proj.nm, "_logCPM.csv")

  r_code <- c(paste0("voom.des <- model.matrix(", voom.model, ", data=pheno)"),
              "dge <- calcNormFactors(dge)")

  if (use_aw){
    r_code <- c(r_code, "elst <- voomWithQualityWeights(dge, voom.des, plot=TRUE)")
  } else {
    r_code <- c(r_code, "elst <- voom(dge, voom.des, plot=TRUE)")
  }

  r_code <- c(r_code, "barplot(dge$samples$norm.factors, names.arg=colnames(dge), las = 2)",
              paste0("write.csv(elst$E, '", norm.filename, "')"))

  norm.txt <- paste0("We normalize samples using the TMM method [@robinson_2010]. This method calculates normalization factors ",
                     "that represent scaling terms for the effective library sizes. The normalization factors range from ",
                       "`r signif(min(dge$samples$norm.factors), 2)` to `r signif(max(dge$samples$norm.factors), 2)`. ",
                       "If no normalization was needed, all factors would be one. ",
                       "We use the normalization factors to calculate log2 counts per million (logCPM), and to estimate ",
                       "precision weights given the mean-variance trend, which allows for linear modeling [@law_2014]. ",
                       "The normalized matrix of logCPM is at ", rmd_links(norm.filename, path=path), ".")

  if (use_aw){
    norm.txt <- c(norm.txt, "",
                  paste0("The precision weights incorporated sample quality weights, which were unbiasedly estimated ",
                         "in the R package limma [@ritchie_2006]. Weights here vary from ",
                         "`r signif(min(elst$target$sample.weights), 2)` to `r signif(max(elst$target$sample.weights), 2)`."))
    sw.r <- c("aw <- setNames(elst$targets$sample.weights, nm=rownames(elst$targets))",
      "barplot(aw, las=3, main='Sample quality weights')", "abline(h=1, lty=2)")
  }

  chunk <- c("# Normalize", "```{r norm, include=TRUE, fig.height=4}", r_code, "```", "")
  if (use_aw){
    chunk <- c(chunk, "```{r aw_bar, include=TRUE, fig.height=4}", sw.r, "```", "")
  }
  chunk <- c(chunk, norm.txt)

  return(chunk)
}
