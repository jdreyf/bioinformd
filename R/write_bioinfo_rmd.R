#' Write Bioinfo RMD
#'
#' Write Bioinformatics RMD workflow
#' @param filename Filename of RMD.
#' @param title Title of RMD.
#' @param path Path of RMD.
#' @export

write_bioinfo_rmd <- function(filename, title, path=NULL){
  sink(paste0(filename, ".Rmd"))
  write_yaml_header(title=title)
  write_setup_chunk(path=path)
  sink()
}
