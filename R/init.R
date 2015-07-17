#' Default idfile for ssh to remote hosts.
#'
#' Availabe at \code{system.file("conduit.key", package = "conduit")}
#'
#' @details Corresponding public key at
#' \code{system.file("conduit.key.pub", package = "conduit")}
defaultIdfile <- system.file("conduit.key", package = "conduit")

## Probably unique conduit session ID
sessionID <- basename(tempfile("conduit"))

#' Character used to separate search paths
#'
#' @seealso \code{resolveRef}
pathSep <- "|"

#' The default Search Paths
#' 
#' @details \itemize{
#' 
#'   \item \dQuote{.//} - the directory containing the file which has
#' initiated the search
#'
#'   \item \dQuote{${ROOT}} - the directory from which the glue system
#' has been invoked }
defaultSearchPaths <- paste(".//", "${ROOT}", sep=pathSep)

