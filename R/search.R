### these functions relate to searching for files to be loaded as modules,
### pipelines, module sources, [...?]

## the character used to separate search paths
pathSep <- "|"

## the default Search Paths
## ".//" - the directory containing the file which has initiated the search
## "${ROOT}" - the directory from which the glue system has been invoked
defaultSearchPaths <- paste(".//", "${ROOT}", sep=pathSep)

## split search paths string
splitPaths <- function(x) {
    strsplit(x, "|", fixed=TRUE)[[1]]
}

## amend paths to be searched
amendSearchPaths <- function(searchPaths, newPaths) {
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
}

searchPaths <- defaultSearchPaths
newPaths <- "|~/Desktop|~/Pictures"
searchPaths <- amendSearchPaths(searchPaths, newPaths)
splitPaths(searchPaths)
