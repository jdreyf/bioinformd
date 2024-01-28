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

roast_contrasts_chunk <- function(grp.var, path, gmt_abbrev=c('reactome', 'tft', 'mir'),
                                  gmt_prefix=c('c2.cp.reactome', 'c3.tft.gtrd', 'c3.mir.mirdb'),
                                  use_des=FALSE, use_aw=TRUE, use_trend=FALSE, elst=FALSE){

  rc.r <- c(paste0("pdb.files <- c(", paste0(gmt_abbrev, "='", gmt_prefix, "'", collapse=", "), ")"),
            paste0("for (i in seq_along(pdb.files)){"),
  "\tG <- read_gmt(paste0('B:/annotations/gene_sets/', pdb.files[i], '.symbols.gmt'))")

  if (elst){
    rc.r <- c(rc.r,
      paste0("\tpwys.fry <- roast_contrasts(elst, G=G, feat.tab = mtt.df, pheno[,'", grp.var, "'], contrast.v=contr.v, fun='fry'"))
  } else {
    rc.r <- c(rc.r, paste0("\tpwys.fry <- roast_contrasts(mtrx, G=G, feat.tab = mtt.df, pheno[,'", grp.var, "'], contrast.v=contr.v,",
            "fun='fry', trend=", use_trend))
  }

  if (use_des) rc.r[4] <- paste0(rc.r[4], ", design=des")
  if (use_aw) rc.r[4] <- paste0(rc.r[4], ", weights=aw")
  rc.r[4] <- paste0(rc.r[4], ", name = names(pdb.files)[i])")
  rc.r[5] <- paste0("\tdotplot_pwys(pwys.fry, cut.sig = 0.25, ntop = 50, name=paste0(names(pdb.files)[i], '_pwys_fry'))")
  rc.r <- c(rc.r, "}")

  rc.txt <- c("We test differential abundance of pathways using limma roast [@wu_2010] for pathways whose analytes",
              "coordinately go up together or coordinately go down together. We also test if there is an",
              "enrichment of analytes that change, even if some go up and others go down -- the *Mixed* test.",
              "",
              "We download pathway databases via the Broad Institute's",
              "[Molecular Signature Database](http://www.gsea-msigdb.org/gsea/msigdb/collections.jsp)[@liberzon_2011].",
              "The results are at", paste0(rmd_links(filenames = paste0(gmt_abbrev, '_fry.xlsx'), path = path), collapse=", "),
              ". The columns give the number of genes in the set, the direction of the gene set, the proportion of genes up-regulated at p<0.05",
              "the proportion of genes down-regulated at p<0.05, and the p-value and FDR for testing if the gene set is coordinately up/down",
              "and for the Mixed test.")

  chunk <- c("# Test pathways", "", "```{r rc}", rc.r, "```", "", rc.txt)
  return(chunk)
}
