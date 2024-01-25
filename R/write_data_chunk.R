#' Write data matrix
#'
#' Write imputed, filtered, normalized data for plotting.
#'
#' @inheritParams limma_contrasts_chunk
#' @export

write_data_chunk <- function(proj.nm, use_annot=TRUE, row.type="gene"){
  wr.header <- "# Write data"
  wr.r <- paste0("write.csv(data.frame(mtrx, annot[rownames(mtrx),]), '", proj.nm, "_", row.type, "_data.csv')")
  wr.txt <- paste0("We write the normalized data to ", rmd_links(filenames = paste0(proj.nm, "_", row.type, '_data.csv')), ".")
  chunk <- c(wr.header, "```{r lc}", wr.r, "```", wr.txt)
  return(chunk)
}
