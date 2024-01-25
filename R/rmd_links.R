#' Create RMD links
#'
#' Create RMD links using jdcbioinfo::make_file_links.
#'
#' @param filenames Filenames to link to, possibly named.
#' @param path Path to prefix. Currently ignored.
#' @export

rmd_links <- function(filenames, path=NULL){
  # if (any(is.null(names(filenames)))){
  #   null.bool <- is.null(names(filenames))
  #   names(filenames) <- filenames[null.bool]
  # }
  # add "/" to path if not null & missing /
  # if (!is.null(path)){
  #   last.chr <- substr(x=path, start=nchar(path), stop=nchar(path))
  #   if (last.chr!="/") path <- paste0(path, "/")
  # }
  # links <- paste0("[", names(filenames), "](", path, filenames, ")")
  links <- paste0("`r make_file_links(wd, '", filenames, "')`")
  return(links)
}
