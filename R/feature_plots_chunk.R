#' Feature plots chunk
#'
#' Feature plots chunk.
#'
#' @param grp.var Variable name in \code{pheno} for group.
#' @param path Path of RMD.
#' @param use_annot Should \code{annot} be used.
#' @export

feature_plots_chunk <- function(grp.var="grp", path, use_annot=TRUE){
  fp.r <- c("signif_hist(mtt, name='signif_hist')",
            "multi_volcano(mtt)",
            "top.feats <- rownames(mtt)[1:min(200, nrow(mtt))]",
            "ezheat(mtrx[top.feats[1:50],], pheno.df=pheno)",
            paste0("plot_by_grp(mtrx[top.feats,], grp=pheno[,'", grp.var, "'])"))

  fp.txt <- c("## Plot rows",
              paste0("We plot the features from the analysis. The histograms of significance are at ",
                     rmd_links(filenames = 'signif_hist.pdf', path = path), ". The volcanoes are at ",
                     rmd_links(filenames = 'volcanoes.pdf', path = path), ", the heatmap is at ",
                     rmd_links(filenames='topgenes_heat.pdf', path = path), ", and the dotplots are at ",
                     rmd_links(filenames='top_genes_dotplots.pdf', path = path), "."))
  chunk <- c(fp.txt, "", "```{r fp}", fp.r, "```")
  return(chunk)
}
