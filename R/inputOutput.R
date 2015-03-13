#' Create an internal IO object
#'
#' @param symbol Name of symbol identifier
#' @return named \code{internalIO} list item
#' @seealso \code{moduleIO}
internalIO <- function(symbol) {
    internalIO <- list(symbol = symbol)
    class(internalIO) <- "internalIO"
    return(internalIO)
}

#' Create a file IO object
#'
#' @param ref Absolute or relative location of file
#' @param path Optional search path for locating file
#' @seealso \code{moduleIO}
fileIO <- function(ref, path = NULL) {
    fileIO <- list(ref = ref, path = path)
    class(fileIO) <- "fileIO"
    return(fileIO)
}
