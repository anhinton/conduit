#' Create an internal IO object
#'
#' @param symbol Name of symbol identifier
#' @return named \code{internalIO} list item
#' @seealso \code{moduleInput}
internalIO <- function(symbol) {
    internalIO <- c(symbol = symbol)
    class(internalIO) <- "internalIO"
    return(internalIO)
}
