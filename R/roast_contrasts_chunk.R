#' Roast contrasts chunk
#'
#' Roast contrasts chunk.
#'
#' @param grp.var Variable name in \code{pheno} for group.
#' @param path Path of RMD.
#' @param pdb.files Text defining pathway database files.
#' @param use_des Logical indicating if design (from limma chunk) should be used.
#' @param use_aw Logical indicating if array weights should be used.
#' @param use_trend Logical indicating if \code{limma trend} should be used.
#' @export

roast_contrasts_chunk <- function(grp.var, path, pdb.files="c(cp = 'c2.cp.v6.0')", use_des=FALSE, use_aw=TRUE,
                                  use_trend=FALSE){
  rc.r <- c(paste0("pdb.files <- ", pdb.files),
            paste0("for(i in seq_along(pdb.files)) {"),
            "G <- read_gmt(paste0('B:/annotations/gene_sets/', pdb.files[i], '.symbols.gmt'))",
paste0("pwys.fry <- roast_contrasts(mtrx, G=G, stats.tab = mtt.df, pheno[,'", grp.var, "'], contrast.v=contr.v,
fun='fry', trend=", use_trend))
  if (use_des) rc.r[4] <- paste0(rc.r[4], ", design=des")
  if (use_aw) rc.r[4] <- paste0(rc.r[4], ", weights=aw")
  rc.r[4] <- paste0(rc.r[4], ", name = names(pdb.files)[i])")
  rc.r <- c(rc.r, "}")

  rc.txt <- c("## Test pathways",
              "We test differential abundance of pathways using limma roast [@roast].")

  chunk <- c(rc.txt, "", "```{r rc}", rc.r, "```")
  return(chunk)
}
