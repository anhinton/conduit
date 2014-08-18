#' Fetch the contents of a referenced file
#'
#' @param ref Address of referenced file
#' @param path File paths to search
#' @return Character vector containing the contents of \code{ref}
#' @import XML
fetchRef <- function(ref, path=defaultSearchPaths) {
    if (grepl("^ *https://", ref)){
        getURL(ref)
    } else if (grepl("^ */", dirname(ref))) {
        readLines(ref)
    } else {
        readLines(findFile(ref, path))
    }
}
