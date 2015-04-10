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
        warning("'ref' is not a length 1 charachter vector")
        warning("coercing 'ref' to length 1 character vector")
        ref <- as_length1_char(ref)
    }
    if (!is.null(path)) {
        if (!is_length1_char(path)) {
            warning("'path' is not a length 1 character vector")
            warning("coercing 'path' to length 1 character vector")
            path <- as_length1_char(path)
        }
    }
    fileIO <- list(ref = ref, path = path)
    class(fileIO) <- "fileIO"
    return(fileIO)
}
