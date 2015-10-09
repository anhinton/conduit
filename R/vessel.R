#' Functions to create \code{vessel} objects.
#'
#' Functions to create vessels for \code{moduleInput},
#' \code{moduleOutput}, or \code{moduleSource} objects.
#' 
#' @details \code{vessel} objects are used as sources for
#' \code{moduleInput} and \code{moduleOutput} objects, or as
#' destinations for \code{moduleOutput} objects.
#'
#' \code{internalVessel} objects are used to point to objects
#' internal to a \code{module} script. They are used to pass objects
#' internal to a module's platform to other modules.
#'
#' \code{fileVessel} objects are used to point to files on the local
#' filesystem.
#'
#' \code{urlVessel} objects are used to point to objects accessible
#' via http:// and https:// protocols.
#'
#' \code{scriptVessel} objects are used to store inline module
#' source scripts.
#'
#' @param symbol symbol name of internal object
#' @param ref Absolute or relative location of file for
#'     \code{fileVessel}, URL for \code{urlVessel}
#' @param path search path(s) for fileVessel$ref
#' @param value Module script as character vector
#' 
#' @return \code{vessel} list object
#'
#' @seealso \code{moduleInput}, \code{moduleOutput},
#' \code{moduleSource}
#' 
#' @examples
#' expenses_df <- internalVessel(symbol="expenses")
#' data_file <- fileVessel(ref="~/myFiles/data.csv")
#' url_object <- urlVessel(ref = "https://github.com/anhinton/conduit")
#' source2_script <-
#'     scriptVessel(
#'         value=c("print(\"Hello World!\")",
#'                 "print(\"Goodbye cruel world...\")"))
#' 
#' @name vessel
NULL

#' @export
#' @rdname vessel
internalVessel <- function(symbol) {
    if (!is_length1_char(symbol)) {
        stop("'symbol' is not a length 1 character vector")
    }
    internalVessel <- list(symbol = symbol)
    class(internalVessel) <- c("internalVessel", "vessel")
    return(internalVessel)
}

#' @export
#' @rdname vessel
fileVessel <- function(ref, path = NULL) {
    if (!is_length1_char(ref)) {
        stop("'ref' is not a length 1 character vector")
    }
    if (!is.null(path) && !is_length1_char(path)) {
        stop("'path' is not a length 1 character vector")
    }
    fileVessel <- list(ref = ref, path = path)
    class(fileVessel) <- c("fileVessel", "vessel")
    return(fileVessel)
}

#' @export
#' @rdname vessel
urlVessel <- function(ref) {
    if (!is_length1_char(ref)) {
        stop("'ref' is not a length 1 character vector")
    }
    urlVessel <- list(ref = ref)
    class(urlVessel) <- c("urlVessel", "vessel")
    return(urlVessel)
}

#' @export
#' @rdname vessel
scriptVessel <- function(value) {
    if (!is.character(value)) {
        stop("'value' is not a character vector")
    }
    scriptVessel <- list(value = value)
    class(scriptVessel) <- c("scriptVessel", "vessel")
    return(scriptVessel)
}
