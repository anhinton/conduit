#' Functions to manipulate vessels
#'
#' Functions to specify vessels for \code{moduleInput},
#' \code{moduleOutput}, or \code{moduleSource} objects.
#'
#' @seealso \code{moduleInput}, \code{moduleOutput},
#' \code{moduleSource}
#' 
#' @name vessel
NULL

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

#' Create a \code{fileVessel} object.
#'
#' @details \code{fileVessel} objects are used to point to files on
#' the local filesystem.
#'
#' These can be used to specify:
#'
#' \itemize{
#'   \item{to where a \code{moduleOutput} is to be saved}
#'   \item{from where a \code{moduleInput} is to retrieved}
#'   \item{from where a \code{moduleSource} is to be retrieved}
#' }
#'
#' \code{ref} (and \code{path}, if given), must be character vectors of
#' length 1.
#'
#' @param ref Absolute or relative location of file
#' @param path Optional search path for locating file
#'
#' @return More about \code{vessel} objects, more on \code{fileVessel}
#' \code{vessel} list objects, overview of \code{module} objects.
#' @seealso \code{moduleInput}, \code{moduleOutput}, \code{moduleSource}
#' 
#' 
#' @export
fileVessel <- function(ref, path = NULL) {
    if (!is_length1_char(ref)) {
        stop(paste0("'ref' is not a length 1 character vector"))
    }
    if (!is.null(path)) {
        if (!is_length1_char(path)) {
            stop("'path' is not a length 1 character vector")
        }
    }
    fileVessel <- list(ref = ref, path = path)
    class(fileVessel) <- c("fileVessel", "vessel")
    return(fileVessel)
}
