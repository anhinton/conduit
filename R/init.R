#' @import methods
NULL

#' @importFrom stats start end
NULL

.onLoad <- function(libname, pkgname) {
    # initIdfile()
    initSession()
    initSearch()
}

.conduit.global <- new.env()

initSession <- function () {
    assign("sessionID",
           basename(tempfile("conduit")),
           .conduit.global)
}

initSearch <- function () {
    pathSep <- "|"
    assign("pathSep", pathSep, .conduit.global)
    assign("defaultSearchPaths",
           paste(".//", "${ROOT}", sep = pathSep),
           .conduit.global)
}
