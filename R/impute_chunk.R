#' Impute chunk
#'
#' Impute chunk.
#'
#' @param input.files Character vector of input files. Should be data.matrix, pheno, then optionally annot.
#' @export

impute_chunk <- function(input.files){
  imputed.filename <- paste0(sub(pattern="\\....$", "", input.files[1]), "_imputed.csv")

  impute.txt <- c("## Impute missing values", paste("We impute using k-nearest neighbors (kNN) of each feature.",
                  "The imputed matrix is at", rmd_links(imputed.filename)))

  r_code <- c("wt.mat <- which(is.na(mtrx), arr.ind=TRUE)",
              "mtrx <- impute::impute.knn(mtrx)$data",
              paste0("write.csv(mtrx, '", imputed.filename, "')"))
  chunk <- c(impute.txt, "```{r impute}", r_code, "```")
  return(chunk)
}
