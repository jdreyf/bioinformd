#' Write Bioinfo RMD
#'
#' Write Bioinformatics RMD workflow.
#'
#' @inheritParams bioinfo_rmd_contrasts
#' @details If need to remove a sample, rerun with new \code{input.files}.
#' @export
#' @examples
#' \donttest{
#' bioinfo_rmd_contrasts(filename="new_analysis", input.files = c("counts.csv", "pheno.csv"), contr.v='c(treat="treat-control")')
#' }

bioinfo_rmd_contrasts_voom <- function(filename, local.path=NULL, data.desc="Gene expression",
                              input.files, data.nas=TRUE, grp.var="grp",
                              covars=NULL, aw.model=paste0("~0+", grp.var), use_aw=TRUE, use_trend=FALSE,
                              contr.v, limma.model=NULL, row.type="gene", gmt_abbrev=c('reactome', 'tft'),
                              gmt_prefix=c('c2.cp.reactome', 'c3.tft.gtrd')){
  proj.nm <- sub("analyze_", "", filename)
  yaml.title <- gsub("_", " ", proj.nm)
  yh <- yaml_header(yaml.title=yaml.title)

  if (is.null(local.path)) local.path <- getwd()
  net.path <- sub("B:/", "J:/cores/bioinformatics/", local.path)
  local.path <- sub("B:/", "", local.path)

  sc <- setup_chunk(path=local.path)
  dt <- data_txt(input.files = input.files, path=net.path)
  rd <- read_data_chunk(input.files=input.files, data.logged=TRUE)
  blocks <- list(yaml=yh, setup=sc, data=dt, read=rd)

  blocks[["feat_filt"]] <- feat_filt_voom_chunk(grp.var=grp.var, row.type = row.type)

  blocks[["norm"]] <- norm_voom_chunk(proj.nm=proj.nm, voom.model=aw.model, path=net.path, use_aw=use_aw)

  # norm already writes out normalized data
  # blocks[["wr"]] <- write_data_chunk(proj.nm = proj.nm, use_annot=use_annot, row.type=row.type)

  blocks[["bp"]] <- boxplot_chunk(elist=TRUE)
  blocks[["pca"]] <- pca_chunk(grp.var=grp.var, proj.nm=proj.nm, covars=covars, elst=TRUE)

  use_annot <- ifelse(length(input.files) >= 3, TRUE, FALSE)
  #don't use aw since already in elst
  blocks[["lc"]] <- limma_contrasts_chunk(grp.var=grp.var, contr.v=contr.v, path=net.path, proj.nm=proj.nm,
                                          limma.model=limma.model, use_aw=FALSE, use_trend=use_trend,
                                          use_annot=use_annot, row.type=row.type, elst=TRUE)
  blocks[["fp"]] <- feature_plots_chunk(grp.var=grp.var, path=net.path, proj.nm=proj.nm, contr.v=contr.v,
                                        use_annot=use_annot, elst=TRUE)

  blocks[["rc"]] <- roast_contrasts_chunk(grp.var=grp.var, path=net.path, elst=TRUE, use_aw=FALSE)

  blocks[["check"]] <- check_chunk()

  blocks[["session"]] <- session_chunk()

  blocks[["refs"]] <- "# References"

  #i want text, but not yaml or code, to skip a line after each \n. Easiest to add "" to text.
  write_blocks(filename=paste0(filename, "_0"), blocks = blocks)
}
