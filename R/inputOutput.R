#' Create an internal IO object
#'
#' @param symbol Name of symbol identifier
#' @return named \code{internalIO} list item
#' @seealso \code{moduleIO}
internalIO <- function(symbol) {
    symbol <- as_length1_char(symbol)
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
    if (!is_length1_char(ref)) {
        stop("'ref' is not a length 1 charachter vector")
    }
    if (!is.null(path)) {
        if (!is_length1_char(path)) {
            stop("'path' is not a length 1 character vector")
        }
    }
    fileIO <- list(ref = ref, path = path)
    class(fileIO) <- "fileIO"
    return(fileIO)
}
