#' Create RMD links
#'
#' Create RMD links.
#'
#' @param filenames Filenames to link to, possibly named.
#' @param path Path to prefix.
#' @export

rmd_links <- function(filenames, path=NULL){
  if (any(is.null(names(filenames)))){
    null.ind <- is.null(names(filenames))
    names(filenames) <- filenames[null.ind]
  }
  #add "/" to path if not null
  if (!is.null(path)){
    last.chr <- substr(x=path, start=nchar(path), stop=nchar(path))
    if (last.chr!="/") path <- paste0(path, "/")
  }
  links <- paste0("[", names(filenames), "](", path, filenames, ")")
  return(links)
}
