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
amendSearchPaths <- function(searchPaths, newPaths) {
    searchPaths <- 
        ## if newPaths starts with pathSep, append to searchPaths
        if (grepl(paste0("^[", pathSep, "]"), newPaths)) {
            paste0(searchPaths, newPaths)
            ## else if newPaths ends with pathSep, prepend to searchPaths
        } else if (grepl(paste0("[", pathSep, "]$"), newPaths)) {
            paste0(newPaths, searchPaths)
            ## else return only newPaths
        } else {
            newPaths
        }
    searchPaths
}

## expandSearchPath to absolute file path
expandSearchPaths <- function(s) {
    s <- gsub("${ROOT}", getwd(), s, fixed=TRUE)
    normalizePath(s)
}

findFile <- function (x, searchPaths) {
    searchPaths <- amendSearchPaths(defaultSearchPaths, searchPaths)
    searchPaths <- splitPaths(searchPaths)
    searchPaths <- unique(expandSearchPaths(searchPaths))
    result <- NULL
    count <- 1
    while (is.null(result) && count <= length(searchPaths)) {
        files <- list.files(path=searchPaths[count], pattern=paste0(x,"$"),
                            full.names=TRUE, recursive=TRUE)
        result <-
            if (length(files) > 0) {
                if (length(files) > 1) {
                    warning(paste(paste("Found more than one match for",
                                        x), files, collapse="\n"))
                }
                files[1]
            } else {
                NULL
            }
        count <- count + 1;
    }
    result
}
