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

#' Returns the correct file extension for a platform's 'internal' files
#'
#' @param language language name
#' @return files exension as character as ".EXT"
internalExtension <- function(language) {
    extension <- switch(language,
                        R = ".rds",
                        python = ".pickle",
                        shell = ".txt")
    extension
}
