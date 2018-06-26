#' Write blocks of text and code
#'
#' Write blocks of text and code.
#'
#' @param filename Filename to write to. \code{Rmd} extension is added.
#' @param blocks List of blocks to write.
#' @export

write_blocks <- function(filename, blocks){
  sink(paste0(filename, ".Rmd"))
  for (i in 1:length(blocks)){
    writeLines(c(blocks[[i]], ""), sep="   \n")
  }
  sink()
}
