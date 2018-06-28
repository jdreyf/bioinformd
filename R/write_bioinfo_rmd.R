#' Write Bioinfo RMD
#'
#' Write Bioinformatics RMD workflow.
#'
#' @param filename Filename of RMD.
#' @param yaml.title Title of RMD.
#' @param path Path of RMD.
#' @param data.type Description of expression and phenotype data.
#' @param input.files Character vector of input files.
#' @param data.logged Logical indicating if data has been log2-transformed.
#' @param data.nas Logical indicating if data has NAs.
#' @param min.npergrp Minimum sample size per group.
#' @param grp.var Variable name in \code{pheno} for group.
#' @param covars Variable name in \code{pheno} for covariates.
#' @param aw.model Model formula for \code{arrayWeights}.
#' @export

write_bioinfo_rmd <- function(filename, yaml.title, path=NULL, data.type="Gene expression", input.files, data.logged=TRUE,
                              data.nas=TRUE, min.npergrp=3, grp.var="grp", aw.model=paste0("~0+", grp.var), covars=NULL){
  yh <- yaml_header(yaml.title=yaml.title)
  sc <- setup_chunk(path=path)
  dt <- data_txt(data.type=data.type, input.files = input.files)
  rd <- read_chunk(input.files=input.files, data.logged=data.logged)
  blocks <- list(yaml=yh, setup=sc, data=dt, read=rd)
  if (data.nas){
    blocks[["impute"]] <- impute_chunk(input.files=input.files)
  }
  blocks[["norm"]] <- norm_chunk(input.files=input.files)
  if (data.nas){
    blocks[["feat_filt"]] <- feat_filt_chunk(min.npergrp=min.npergrp)
  }
  blocks[["aw"]] <- sample_weights_chunk(aw.model=aw.model)
  blocks[["bp"]] <- boxplot_chunk()
  blocks[["pca"]] <- pca_chunk(grp.var=grp.var, covars=covars)

  blocks[["refs"]] <- "## References"
  write_blocks(filename=filename, blocks = blocks)
}
