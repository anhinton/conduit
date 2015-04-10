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

#' Create an \code{internalVessel} object.
#'
#' @details \code{internalVessel} objects are used to point to objects
#' internal to a \code{module} script. They are used to pass objects
#' internal to a module's platform to other modules.
#'
#' These can be used to specify:
#'
#' \itemize{
#'   \item{an object created in a \code{module} script to be passed out as
#'         a \code{moduleOutput}}
#'   \item{an object which a \code{module} script expects to be provided
#'         by a \code{moduleInput}}
#' }
#'
#' \code{symbol} must be a character vectors of length 1.
#'
#' @param symbol Name of internal object
#'
#' @return \code{internalVessel}, \code{vessel} list object
#'
#' @seealso More about \code{vessel} objects, more about
#' \code{moduleInput} and \code{moduleOutput} objects, overview of
#' \code{module} objects.
#'
#' @examples
#' expenses_df <- internalVessel(symbol="expenses")
#'  
#' @export
internalVessel <- function(symbol) {
    if (!is_length1_char(symbol)) {
        stop("'symbol' is not a length 1 character vector")
    }
    internalVessel <- list(symbol = symbol)
    class(internalVessel) <- c("internalVessel", "vessel")
    return(internalVessel)
}

#' Create a \code{fileVessel} object.
#'
#' @details \code{fileVessel} objects are used to point to files on
#' the local filesystem.
#'
#' These can be used to specify:
#'
#' \itemize{
#'   \item{where a module script has created a file, as described in
#'         a \code{moduleOutput}}
#'   \item{from where a module script expects a file to be available,
#'         as described in a \code{moduleInput}}
#'   \item{from where a \code{moduleSource} is to be retrieved}
#' }
#'
#' \code{ref} (and \code{path}, if given), must be character vectors of
#' length 1.
#'
#' @param ref Absolute or relative location of file
#' @param path Optional search path for locating file
#'
#' @return \code{fileVessel}, \code{vessel} list object
#'
#' @seealso More about \code{vessel} objects, more about
#' \code{moduleInput}, \code{moduleOutput} and \code{moduleSource}
#' objects, an overview of \code{module} objects.
#'
#' @examples
#' data_file <- fileVessel(ref="~/myFiles/data.csv")
#'  
#' @export
fileVessel <- function(ref, path = NULL) {
    if (!is_length1_char(ref)) {
        stop("'ref' is not a length 1 character vector")
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

#' Create a \code{scriptVessel} object.
#'
#' @details \code{scriptVessel} objects are used to store inline module
#' source scripts.
#'
#' @param value Character vector of module script
#'
#' @return \code{scriptVessel}, \code{vessel} list object
#'
#' @seealso \code{vessel}, \code{moduleSource}, \code{source}
#'
#' @examples
#' source2_script <-
#'     scriptVessel(
#'         value=c("print(\"Hello World!\")",
#'                 "print(\"Goodbye cruel world...\")"))
#'
#' @export
scriptVessel <- function(value) {
    if (!is.character(value)) {
        stop("'value' is not a character vector")
    }
    scriptVessel <- list(value = value)
    class(scriptVessel) <- c("scriptVessel", "vessel")
    return(scriptVessel)
}
