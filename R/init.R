#' Default idfile for ssh to remote hosts
defaultIdfile <- system.file("conduit.key", package = "conduit")

#' Conduit session ID
sessionID <- basename(tempfile("conduit"))

## the character used to separate search paths
pathSep <- "|"

## the default Search Paths
## ".//" - the directory containing the file which has initiated the search
## "${ROOT}" - the directory from which the glue system has been invoked
defaultSearchPaths <- paste(".//", "${ROOT}", sep=pathSep)

