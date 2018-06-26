#' Write Bioinfo RMD
#'
#' Write Bioinformatics RMD workflow.
#'
#' @param filename Filename of RMD.
#' @param title Title of RMD.
#' @param path Path of RMD.
#' @param data.type Description of expression and phenotype data.
#' @param input.files Character vector of input files.
#' @param data.logged Logical indicating if data has been log2-transformed.
#' @param data.nas Logical indicating if data has NAs.
#' @export

write_bioinfo_rmd <- function(filename, title, path=NULL, data.type="Gene expression", input.files, data.logged=TRUE,
                              data.nas=TRUE){
  yh <- yaml_header(title=title)
  sc <- setup_chunk(path=path)
  dt <- data_txt(data.type=data.type, input.files = input.files)
  rd <- read_chunk(input.files=input.files, data.logged=data.logged)
  blocks <- list(yh, sc, dt, rd)

  if (data.nas){
    imp <- impute_chunk(input.files=input.files)
    blocks[[length(blocks)+1]] <- imp
  }

  write_blocks(filename=filename, blocks = blocks)
}
