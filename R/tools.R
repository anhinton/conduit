#' Gets a named XML attribute if it exists
#' 
#' @param xmlNode \code{XMLNode} whose attributes are to be extracted
#' @param attrName String vale naming attribute required
#' @return Character string containing attribute value, or NULL
#' @import XML
getXMLAttr <- function(xmlNode, attrName) {
    allAttrs <- xmlAttrs(xmlNode)
    attr <- if(any(names(allAttrs) == attrName)) {
        allAttrs[[attrName]]
    } else {
        NULL
    }
}

#' Checks whether \code{x} is a length 1 character vector
#'
#' @param x vector to be checked
#' @return logical, TRUE if \code{x} is length 1 char
is_length1_char <- function(x) {
    value <- FALSE
    if (length(x) == 1 && is.character(x)) {
        value <- TRUE
    }
    return(value)
}

#' Tests if a file path is absolute
#'
#' @param path file path
#' @return TRUE if absolute
is_absolute <- function (path) {
    if (dirname(path) == ".") return(FALSE)
    if (regexpr("^~", path) != -1) return(TRUE)
    if (regexpr("^/", path) != -1) return(TRUE)
    if (regexpr("^[[:alpha:]]+:(/|\\\\)", path) != -1) return(TRUE)
    else return(FALSE)
}

#' Do a topological sort on a graph
#'
#' Return a linear ordering of vertices in an acyclic directed graph
#' such that for every directed edge from u to v, u comes before v in
#' the ordering.
#'
#' \code{edges} list should be named for each vertex in the
#' graph. Each vertex object should be a character vector of vectors
#' to which the vector has a directed edge.
#'
#' Uses Kahn's algorithm
#' \url{https://en.wikipedia.org/wiki/Topological_sorting#Kahn.27s_algorithm}
#'
#' @param edges named list of edges from each vertex as character
#'     vector
#'
#' @return character vector of sorted vertices
topologicalSort <- function(edges) {
    ## check all edges connect to a named vertex
    for (i in unlist(edges)) {
        if (!(i %in% names(edges))) {
            stop(paste("vertex", i, "is not defined"))
        }
    }
    
    ## find nodes with no incoming edges
    startNodes <- lapply(
        X = names(edges),
        FUN = function (n, edges) {
            if (!(n %in% edges))
                n
        },
        edges = unique(unlist(edges)))
    startNodes <- unlist(startNodes)

    order <- character()
    while (length(startNodes)) {
        n <- startNodes[1]
        startNodes <- startNodes[-1]
        order <- c(order, n)
        for (i in edges[[n]]) {
            e <- edges[[n]][1]
            edges[[n]] <- edges[[n]][-1]
            if (!(e %in% unlist(edges))) {
                startNodes <- c(startNodes, e)
            }
        }
    }

    ## if any edges remain there must be a cycle
    if (length(unlist(edges)))
        stop ("graph has at least one cycle")

    order
}

#' Copy a directory and its contents
#'
#' @details Copies a directory and all its contents recursively to a
#'     new location. If the \file{to} directory does not exist it will
#'     be created. If it does exist the contens of \file{from} will be
#'     copied into \code{to}. By default contents of \file{to} will
#'     not be overwritten.
#'
#' @param from directory to copy
#' @param to destination directory
#' @param overwrite logical; should existing destination files be
#'     overwritten
#'
#' @return logical vector indicating which files were successfully
#'     copied
dir.copy <- function(from, to, overwrite = FALSE) {
    if (!file.info(from)$isdir)
        stop("from must be a file directory")
    if (file.exists(to) & !file.info(to)$isdir)
        stop("to must be a file directory")
    if (!dir.exists(to))
        dir.create(path = to)
    dirContents <- list.files(
        path = from, all.files = TRUE, full.names = TRUE,
        recursive = TRUE)
    file.copy(from = dirContents, to = to, overwrite = overwrite)
}
