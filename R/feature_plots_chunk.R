#' Feature plots chunk
#'
#' Feature plots chunk.
#'
#' @param grp.var Variable name in \code{pheno} for group.
#' @param path Path of RMD.
#' @param proj.nm Name of project to paste into filenames.
#' @param contrv Named vector of contrasts.
#' @param use_annot Should \code{annot} be used.
#' @param row.type Character in filename for plots.
#' @param elst Logical indicating if expression object an EList.
#' @export

feature_plots_chunk <- function(grp.var="grp", path, proj.nm, contr.v, use_annot=TRUE, row.type="gene", elst=FALSE){
  fp.r <- c(paste0("signif_hist(mtt, name='", proj.nm, "_signif_hist')"),
            paste0("multi_volcano(tab=mtt, name='", proj.nm,  "_volcanoes')"),
            "top.feats <- rownames(mtt)[1:min(200, nrow(mtt))]")

  if (elst){
    fp.r <- c(fp.r, paste0("ezheat(elst$E[top.feats[1:50],], pheno.df=pheno, name='", proj.nm, "_top", row.type, "s_heat')"),
              paste0("plot_by_grp(elst$E[top.feats,], grp=pheno[,'", grp.var, "'], name='", proj.nm, "_top", row.type, "s')"))
  } else {
    fp.r <- c(fp.r, paste0("ezheat(mtrx[top.feats[1:50],], pheno.df=pheno, name='", proj.nm, "_top", row.type, "s_heat')"),
    paste0("plot_by_grp(mtrx[top.feats,], grp=pheno[,'", grp.var, "'], name='", proj.nm, "_top", row.type, "s')"))
  }

  rows.type <- paste0(row.type, "s")
  fp.txt <- c(paste("## Plot", rows.type),
            paste0("We plot the ", rows.type, " from the analysis. If there are multiple comparisons or associations, ",
                   "we select the ", row.type, "s that are most significant across these. However, we can also plot ",
                   "the top ", row.type, "s within an individual analysis, or any ", row.type, "s you are interested in. "), "",
            paste0("The histograms of significance are at ", rmd_links(filenames = paste0(proj.nm, '_signif_hist.pdf'), path = path),
                   ". If no ", row.type, "s were associated with the phenotype, we would expect the p-value histogram ",
                   "to be flat and all FDRs to be near one. The more associated ", row.type, "s there are, the more ",
                   "enrichment there is at low p-values, the lower will be the FDRs. "), "",
            paste0("The volcanoes are at ", rmd_links(filenames = paste0(proj.nm, '_volcanoes.pdf'), path = path),
                   ". Several ", row.type, "s with the lowest p-values or largest fold changes are annotated automatically. "),
            "", paste0("The heatmap is at ", rmd_links(filenames=paste0(proj.nm, '_top', row.type, 's_heat.pdf'), path = path),
                       ". It plots the log2 expression of the 50 most significant ", row.type, "s. "), "",
            paste0("The dotplots of the 200 most significant ", row.type, "s is at ",
                   rmd_links(filenames=paste0(proj.nm, '_top', row.type, 's_dotplots.pdf'), path = path), "."))

  #if mult contrasts, use venn
  if (grepl(",", contr.v[1])){
    fp.r <- c(fp.r, paste0("sig.tab <- ezvenn(mtt, p.cutoff=0.05, name='", proj.nm, "_", row.type, "')"),
              paste0("write.csv(sig.tab, '", proj.nm, "_", row.type, "_venn_tab.csv')"))
    fp.txt <- c(fp.txt, "", paste0("The Venn diagram is at ", rmd_links(filenames = paste0(proj.nm, "_", row.type, "_venn.pdf"),
                 path = path), ". The Venn table is at ", rmd_links(filenames=paste0(proj.nm, "_", row.type, '_venn_tab.csv'),
                                                                    path = path),
                 ". It has elements {-1, 0, 1}. For a ", row.type, " in a comparison, 0 indicates no significant change; ",
                 "-1 indicates down; and 1 indicates up."))
  }

  chunk <- c(fp.txt, "", "```{r fp}", fp.r, "```")
  return(chunk)
}
