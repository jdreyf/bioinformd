#' Write Bioinfo RMD
#'
#' Write Bioinformatics RMD workflow.
#'
#' @param filename Filename of RMD.
#' @param path Path of RMD.
#' @param data.desc Description of data sources for \code{input.files}.
#' @param input.files Character vector of input files. Matrix, pheno, and (optionally) annot.
#' @param data.logged Logical indicating if data has been log2-transformed.
#' @param data.nas Logical indicating if data has NAs.
#' @param min.npergrp Minimum sample size per group.
#' @param grp.var Variable name in \code{pheno} for group.
#' @param covars Variable name in \code{pheno} for covariates.
#' @param use_aw Logical indicating if array weights should be used.
#' @param use_trend Logical indicating if \code{limma trend} should be used.
#' @param contr.v Text defining named vector of contrasts.
#' @param limma.model Model formula for \code{limma}, if want a design matrix.
#' @param row.type Character in filename for features.
#' @param pdb.files Text defining pathway database files.
#' @details If need to remove a sample, rerun with new \code{input.files}.
#' @export

bioinfo_rmd_contrasts <- function(filename, local.path=NULL, data.desc="Gene expression",
                              input.files, data.logged=TRUE, data.nas=TRUE, min.npergrp=3, grp.var="grp",
                              covars=NULL, aw.model=paste0("~0+", grp.var), use_aw=TRUE, use_trend=FALSE,
                              contr.v=NULL, limma.model=NULL, row.type="gene", pdb.files){
  proj.nm <- sub("analyze_", "", filename)
  yaml.title <- gsub("_", " ", proj.nm)
  yh <- yaml_header(yaml.title=yaml.title)

  if (is.null(local.path)) local.path <- getwd()
  net.path <- sub("B:/|/n/bioinformatics/", "//jdcfs1/cores/bioinformatics/", local.path)
  local.path <- sub("B:/|/n/bioinformatics/", "", local.path)

  sc <- setup_chunk(path=local.path)
  dt <- data_txt(data.type=data.desc, input.files = input.files, path=net.path)
  rd <- read_chunk(input.files=input.files, data.logged=data.logged)
  blocks <- list(yaml=yh, setup=sc, data=dt, read=rd)
  if (data.nas){
    blocks[["impute"]] <- impute_chunk(input.files=input.files, path=net.path)
  }
  blocks[["norm"]] <- norm_chunk(input.files=input.files, path=net.path)
  if (data.nas){
    blocks[["feat_filt"]] <- feat_filt_chunk(min.npergrp=min.npergrp, row.type = row.type)
  }
  aw.model <- paste0("~0+", grp.var)
  blocks[["aw"]] <- sample_weights_chunk(aw.model=aw.model)
  blocks[["bp"]] <- boxplot_chunk()
  blocks[["pca"]] <- pca_chunk(grp.var=grp.var, proj.nm=proj.nm, covars=covars)
  blocks[["mvt"]] <- meanvar_trend_chunk(voom.model=aw.model, use_trend=use_trend)

  use_annot <- ifelse(length(input.files) >= 3, TRUE, FALSE)
  blocks[["lc"]] <- limma_contrasts_chunk(grp.var=grp.var, contr.v=contr.v, path=net.path, proj.nm=proj.nm, limma.model=limma.model,
                                          use_aw=use_aw, use_trend=use_trend, use_annot=use_annot, row.type=row.type)
  blocks[["fp"]] <- feature_plots_chunk(grp.var=grp.var, path=net.path, proj.nm=proj.nm, contr.v=contr.v, use_annot=use_annot)

  blocks[["rc"]] <- roast_contrasts_chunk(grp.var=grp.var, path=net.path)

  blocks[["refs"]] <- "## References"
  write_blocks(filename=filename, blocks = blocks)
}
