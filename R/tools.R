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

#' Checks whether /code{x} is a length 1 character vector
#'
#' @param x vector to be checked
#' @return logical, TRUE if is length 1 char
is_length1_char <- function(x) {
    value <- FALSE
    if (length(x) == 1 && is.character(x)) {
        value <- TRUE
    }
    return(value)
}

#' Coerce a vector to a character vector of length 1
#'
#' @param x vector to be coerced
#' @return character vector of length one
as_length1_char <- function(x) {
    if (length(x) > 1) {
        x <- x[1]
    }
    if (!is.character(x)) {
        x <- as.character(x)
    }
    return(x)
}
