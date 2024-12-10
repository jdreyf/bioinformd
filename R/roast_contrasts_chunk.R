#' Roast contrasts chunk
#'
#' Roast contrasts chunk.
#'
#' @param grp.var Variable name in \code{pheno} for group.
#' @param path Path of RMD.
#' @param use_des Logical indicating if design (from limma chunk) should be used.
#' @param use_aw Logical indicating if array weights should be used.
#' @param use_trend Logical indicating if \code{limma trend} should be used.
#' @param elst Logical indicating if expression object an EList.
#' @inheritParams bioinfo_rmd_contrasts
#' @export

roast_contrasts_chunk <- function(grp.var, path, gmt_abbrev=c('reactome', 'gtrd', 'mirdb'),
                                  gmt_prefix=c('c2.cp.reactome', 'c3.tft.gtrd', 'c3.mir.mirdb'),
                                  use_des=FALSE, use_aw=TRUE, use_trend=FALSE, elst=FALSE){

  rc.r <- c(paste0("pdb.files <- c(", paste0(gmt_abbrev, "='", gmt_prefix, "'", collapse=", "), ")"),
            paste0("for (i in seq_along(pdb.files)){"),
  "\tG <- read_gmt(paste0('B:/annotations/gene_sets/', pdb.files[i], '.symbols.gmt'))",
  "\tG <- map_glist(G, annot=annot)")

  if (elst){
    rc.r <- c(rc.r,
      paste0("\tpwys.fry <- roast_contrasts(elst, G=G, feat.tab = mtt.df, pheno[,'", grp.var, "'], contrast.v=contr.v, fun='fry'"))
  } else {
    rc.r <- c(rc.r, paste0("\tpwys.fry <- roast_contrasts(mtrx, G=G, feat.tab = mtt.df, grp=pheno[,'", grp.var, "'], contrast.v=contr.v,",
            "fun='fry', trend=", use_trend))
  }

  fry.ind <- grep(pattern = "roast_contrasts", x=rc.r)
  stopifnot(length(fry.ind) == 1)
  if (use_des) rc.r[fry.ind] <- paste0(rc.r[fry.ind], ", design=des")
  if (use_aw) rc.r[fry.ind] <- paste0(rc.r[fry.ind], ", weights=aw")
  rc.r[fry.ind] <- paste0(rc.r[fry.ind], ", name = names(pdb.files)[i])")
  rc.r <- c(rc.r,
    "\tsignif_hist(pwys.fry, name = paste0(names(pdb.files)[i], '_fry_signif_hist'))",
    "\tdotplot_pwys(pwys.fry, cut.sig = 0.25, type.sig='FDR', ntop = 50, name=paste0(names(pdb.files)[i], '_fry'))",
    "\tbubbleplot_pwys(pwys.fry, name=paste0(names(pdb.files)[i], '_fry'))",
    "}")

  rc.txt <- glue("We test differential abundance of pathways using limma roast [@wu_2010] for pathways whose analytes coordinately go up together or \\
                   coordinately go down together. We also test if there is an enrichment of analytes that change, even if some go up and others go down -- the *Mixed* test. \\
                   The benefit of the Mixed test is that it will detect a pathway where half of the genes are significantly upregulated and the other half are equally \\
                   significantly downregulated, whereas the non-mixed test would conclude this pathway is not significant, because the upregulated genes would cancel out the \\
                   downregulated genes.

                  We download pathway databases via the Broad Institute's \\
                  [Molecular Signature Database](http://www.gsea-msigdb.org/gsea/msigdb/collections.jsp)[@liberzon_2011]. \\
                  We test pathways derived from [Reactome](https://reactome.org/PathwayBrowser/), microRNA target predictions from [mirdb](https://mirdb.org/), \\
                  and transcription factor targets from the [Gene Transcription Regulation DB](https://gtrd.biouml.org/#!).

                  The results are at `r make_file_links(wd, '_fry.xlsx', recursive=TRUE)`. \\
                  The first column gives hyperlinked pathway names. If you click on these links, they should take you to the statistics of the genes in the pathway. \\
                  If the first column does not have these names, it could be because the Excel file lives in a zipped folder and the solution is to unzip the folder. \\
                  The next columns give the number of genes in the set, the direction of the gene set, the proportion of genes up-regulated at p<0.05 \\
                  the proportion of genes down-regulated at p<0.05, and the p-value and FDR for testing if the gene set is coordinately up/down and for the Mixed test.

                  The histograms of significance are shown in `r make_file_links(wd, 'fry_signif_hist\\.pdf')`. \\
                  If no pathways were associated with the phenotype, we would expect the *p*-value histogram to be flat and all FDRs to be near one. \\
                  The more associated pathways there were, the more enrichment there was at low *p*-values, the lower will be the FDRs.

                  A dot plot of the top pathways with FDR < 25% is at `r make_file_links(wd, '_dotplot.pdf')`.

                  We made bubble plots per comparison and direction where the x-axis is proportion of genes with p-value < 5%, the y-axis shows the top pathways whose p-value < 5%, \\
                  the dot color shows the pathway significance, and the dot size shows the number of genes in the pathway with p-value < 5% (although it's possible a \\
                  significant pathway doesn't contain any significant genes, since it's sufficient if all genes in a pathway change coordinately). The bubble plots are \\
                  at `r make_file_links(pwd, 'fry_bubbleplots\\.pdf')`.
                 ")

  chunk <- c("# Test pathways", "", "```{r rc}", rc.r, "```", "", rc.txt)
  return(chunk)
}
