#' Information about file searching in \code{conduit}.
#' 
#' @name searching
NULL

### these functions relate to searching for files to be loaded as modules,
### pipelines, module sources, [...?]

#' Split a search paths string separated by \code{pathSep}
#'
#' @param s Search paths string
#' @return A list of paths
splitPaths <- function(s) {
    strsplit(s, pathSep, fixed=TRUE)[[1]]
}

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

#' Expand a search path to an absolute path
#'
#' @param searchPaths search paths
#' @param location file directory of invoking pipeline/module xml (optional)
#' @return absolute path as character string
expandSearchPaths <- function(searchPaths, location = getwd()) {
    searchPaths <- gsub("${ROOT}", getwd(), searchPaths, fixed=TRUE)
    searchPaths <- gsub(".//", location, searchPaths, fixed=TRUE)
    normalizePath(searchPaths)
}

#' Find a file referenced by \code{ref} and \code{path}
#'
#' @param ref file path or filename
#' @param path search path (optional)
#' @param location file directory of invoking pipeline/module xml (optional)
#' @return absolute file path as character or NULL
findFile <- function (ref, path = NULL, location = getwd()) {
    result <- NULL
    if (file.exists(ref) && ref == normalizePath(ref)) {
        ## if ref is an absolute path
        result <- ref
    } else if (file.exists(ref) && normalizePath(ref) == path.expand(ref)) {
        ## if ref is a path relative to $HOME
        result <- normalizePath(ref)
    } else if (grepl("^../", ref) && file.exists(file.path(location, ref))) {
        ## if ref is relative to `location`
        result <- normalizePath(file.path(location, ref))
    } else {
        searchPaths <-
            if (is.null(path)) {
                defaultSearchPaths
            } else {
                amendSearchPaths(path)
            }
        searchPaths <- splitPaths(searchPaths)
        searchPaths <- unique(expandSearchPaths(searchPaths, location))
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

#' Resolves a full path for a given ref and path
#'
#' @param ref address/filename of referenced file
#' @param path file paths to search
#' @param location file directory of invoking pipeline/module xml
#'
#' @return character string of resolved ref with class set to
#' appropriate read method
resolveRef <- function (ref, path = NULL, location = getwd()) {
    if (grepl("^ *https://", ref)) {
        ref <- ref
        class(ref) <- "https"
    } else if (grepl("^ *http://", ref)) {
        ref <- ref
        class(ref) <- "http"
    } else {
        ref <- findFile(ref, path, location)
        class(ref) <- "file"
    }
    return(ref)
}

#' Read the contents of a referenced file
#' 
#' @details \code{file} should be the result of the function
#' \code{resolveRef}. The class of this object determines which read
#' method is used.
#'
#' @seealso \code{resolveRef}
#' 
#' @param file character vector containing resolved ref location
#' @return Character vector containing the contents of \code{file}
fetchRef <- function (file) {
    UseMethod("fetchRef")
}

fetchRef.https <- function (file) {
    RCurl::getURL(file)
}

fetchRef.default <- function (file) {
    readLines(file)
}
