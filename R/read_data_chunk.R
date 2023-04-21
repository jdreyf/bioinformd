#' Read chunk
#'
#' Read chunk
#'
#' @param input.files Character vector of input files. Should be data matrix, pheno, then optionally annot.
#' @param data.logged Logical indicating if data has been log2-transformed.
#' @export

read_data_chunk <- function(input.files, data.logged){
  r_code <- paste0("mtrx <- read.csv('", input.files[1], "', row.names = 1)")
  if (!data.logged){
    r_code <- c(r_code, "mtrx <- log2(mtrx)")
  }
  r_code <- c(r_code, paste0("pheno <- read.csv('", input.files[2], "', row.names = 1)"))
  if (length(input.files)==3){
    r_code <- c(r_code, paste0("annot <- read.csv('", input.files[3], "', row.names = 1)"))
  }

  chunk <- c("```{r read}", r_code, "```")
  return(chunk)
}
