### these functions relate to searching for files to be loaded as modules,
### pipelines, module sources, [...?]

## the character used to separate search paths
pathSep <- "|"

## the default Search Paths
## ".//" - the directory containing the file which has initiated the search
## "${ROOT}" - the directory from which the glue system has been invoked
defaultSearchPaths <- paste(".//", "${ROOT}", sep=pathSep)

## split search paths string
splitPaths <- function(s) {
    strsplit(s, pathSep, fixed=TRUE)[[1]]
}

## amend paths to be searched
#' Amend search paths
#'
#' Append, prepend, or replace search paths with a new set.
#'
#' If \code{newPaths} ends with the | character, new search paths will be
#' prepended to \code{pathsToAmend}.
#'
#' If \code{newPaths} begins with | character, new search paths will be
#' appended to \code{pathsToAmend}.
#'
#' Otherwise \code{newPaths} replaces \code{pathsToAmend}.
#'
#' @param newPaths Character string of paths to be added
#' @param pathsToAmend Character string of paths to be amended
#' @return Character string of amended search paths
amendSearchPaths <- function(newPaths, pathsToAmend = defaultSearchPaths) {
    amendedPaths <- 
        ## if newPaths starts with pathSep, append to pathsToAmend
        if (grepl(paste0("^[", pathSep, "]"), newPaths)) {
            paste0(pathsToAmend, newPaths)
            ## else if newPaths ends with pathSep, prepend to pathsToAmend
        } else if (grepl(paste0("[", pathSep, "]$"), newPaths)) {
            paste0(newPaths, pathsToAmend)
            ## else return only newPaths
        } else {
            newPaths
        }
    amendedPaths
}

## expandSearchPath to absolute file path
expandSearchPaths <- function(s) {
    s <- gsub("${ROOT}", getwd(), s, fixed=TRUE)
    normalizePath(s)
}

findFile <- function (ref, path = NULL) {
    result <- NULL
    ## check if ref is an absolute path
    if (file.exists(ref) && ref == normalizePath(ref)) {
        ## if ref is an absolute path
        result <- ref
    } else if (file.exists(ref) && ref != path.expand(ref)) {
        ## if ref is a path relative to $HOME
        result <- normalizePath(ref)
    } else {
        searchPaths <-
            if (is.null(path)) {
                defaultSearchPaths
            } else {
                amendSearchPaths(path)
            }
        searchPaths <- splitPaths(searchPaths)
        searchPaths <- unique(expandSearchPaths(searchPaths))
        if (grepl("^[.]{2}", ref)[1]) {
            ## if ref is relative, calculate search paths
            relativeStart <- substr(ref, 1, regexpr("[/][^.]", ref) - 1)
            ref <- substr(ref, regexpr("[/][^.]", ref) + 1, nchar(ref))
            searchPaths <-
                sapply(searchPaths,
                       function (p, relStart) {
                           owd <- setwd(p)
                           on.exit(setwd(owd))
                           setwd(relStart)
                           getwd()
                       }, relativeStart)
        }
        count <- 1
        while (is.null(result) && count <= length(searchPaths)) {
            filesInPath <- list.files(path=searchPaths[count], recursive=TRUE,
                                      full.names=TRUE)
            whichFiles <- grep(paste0(ref, "$"), filesInPath)
            files <- filesInPath[whichFiles]
            result <-
                if (length(files) > 0) {
                    if (length(files) > 1) {
                        warning(paste(paste("Found more than one match for",
                                            ref), files, collapse="\n"))
                    }
                    files[1]
                } else {
                    NULL
                }
            count <- count + 1;
        }
    }
    result
}

#' Fetch the contents of a referenced file
#'
#' @param ref Address of referenced file
#' @param path File paths to search
#' @return Character vector containing the contents of \code{ref}
#' @import XML
fetchRef <- function(ref, path = NULL) {
    if (grepl("^ *https://", ref)){
        getURL(ref)
    ## } else if (grepl("^ */", dirname(ref))) {
    ##     readLines(ref)
    } else {
        filePath <- findFile(ref, path)
        if (is.null(filePath)) {
            stop(paste0("Unable to find file with ref='", ref, "' path='",
                        path, "'"))
        } else {
            readLines(findFile(ref, path))
        }
    }
}
