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

#' Coerce a vector to a character vector of length one and warn about
#' coercion.
#'
#' @param x vector to be coerced
#' @return character vector of length one
as_length1_char <- function(x) {
    if (length(x) > 1) {
        warning(paste0("More than one value provided. ",
                       "Using only the first value."))
        x <- x[1]
    }
    if (!is.character(x)) {
        warning(paste0("Non-character string provided. ",
                      "Coercing to string."))
        x <- as.character(x)
    }
    return(x)
}
