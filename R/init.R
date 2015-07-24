.onLoad <- function(libname, pkgname) {
    initIdfile()
    initSession()
    initSearch()
}

.conduit.global <- new.env()

initIdfile <- function () {
    Sys.chmod(system.file("conduit.key", package = "conduit"), mode = "0400")
    assign("defaultIdfile",
           system.file("conduit.key", package = "conduit"),
           .conduit.global)
}

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
