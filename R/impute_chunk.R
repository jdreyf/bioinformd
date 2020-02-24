#' Impute chunk
#'
#' Impute chunk.
#'
#' @param input.files Character vector of input files. Should be data.matrix, pheno, then optionally annot.
#' @param path Path of RMD.
#' @param row.type Character in filename for features.
#' @export

impute_chunk <- function(input.files, path, row.type="gene"){
  imputed.filename <- paste0(sub(pattern="\\....$", "", input.files[1]), "_imputed.csv")

  impute.txt <- c("## Impute missing values", paste0("We impute using half the minimum non-missing value of each ", row.type,
                    ". The imputed matrix is at ", rmd_links(imputed.filename, path=path), "."))

  r_code <- c("mtrx.na <- mtrx",
              "# Assume data is logged",
              "mtrx <- t(apply(mtrx.na, MARGIN=1, FUN=function(v){ v[is.na(v)] <- min(v[!is.na(v)]) - 1; v }))",
              paste0("write.csv(mtrx, '", imputed.filename, "')"))
  chunk <- c(impute.txt, "```{r impute}", r_code, "```")
  return(chunk)
}
