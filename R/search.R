### these functions relate to searching for files to be loaded as modules,
### pipelines, module sources, [...?]

## the character used to separate search paths
pathSep <- "|"

## the default Search Path
## ".//" - the directory containing the file which has initiated the search
## "${ROOT}" - the directory from which the glue system has been invoked
defaultSearchPath <- paste(".//", "${ROOT}", sep=pathSep)
