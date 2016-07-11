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
    strsplit(s, get("pathSep", envir = .conduit.global), fixed=TRUE)[[1]]
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
amendSearchPaths <-
    function(newPaths,
             pathsToAmend = get("defaultSearchPaths",
                 envir = .conduit.global)) {
    amendedPaths <- 
        ## if newPaths starts with get("pathSep", envir = .conduit.global),
        ## append to pathsToAmend
        if (grepl(paste0("^[", get("pathSep", envir = .conduit.global), "]"),
                  newPaths)) {
            paste0(pathsToAmend, newPaths)
            ## else if newPaths ends with
            ## get("pathSep", envir = .conduit.global), prepend to pathsToAmend
        } else if (grepl(paste0("[", get("pathSep", envir = .conduit.global),
                                "]$"), newPaths)) {
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
    } else if (file.exists(file.path(location, ref))) {
        ## if ref is relative to `location`
        result <- normalizePath(file.path(location, ref))
    } else {
        searchPaths <-
            if (is.null(path)) {
                get("defaultSearchPaths", envir = .conduit.global)
            } else {
                amendSearchPaths(path)
            }
        searchPaths <- gsub(pattern = "[.]//", replacement = location,
                            x = searchPaths)
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
