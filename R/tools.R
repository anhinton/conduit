#' Fetches an XML attribute if it exists
#' 
#' @param xmlNode \code{XMLNode} whose attributes are to be extracted
#' @param attrName String vale naming attribute required
#' @return Character string containing attribute value, or NULL
#' @import XML
fetchXMLAttr <- function(xmlNode, attrName) {
    allAttrs <- xmlAttrs(xmlNode)
    attr <- if(any(names(allAttrs) == attrName)) {
        allAttrs[[attrName]]
    } else {
        NULL
    }
}
