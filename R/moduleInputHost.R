#' Create a \code{moduleInputHost} object
#'
#' Create a \code{moduleHost} object of class \code{moduleInputHost}
#' to indicate that a \code{moduleInput} will provide the real
#' \code{moduleHost} at execution.
#'
#' @param name Name of \code{moduleInput}
#'
#' @return Object of class \code{moduleInputHost} and \code{moduleHost}
#'
#' @export
moduleInputHost <- function(name) {
    if (!is_length1_char(name))
        stop("name must be a length one character string")
    moduleInputHost <- list(name = name)
    class(moduleInputHost) <- c("moduleInputHost", "moduleHost")
    moduleInputHost
}

#' Create a \code{moduleInputHost} object from <host><moduleInput/></host> XML
#'
#' @param moduleInputHostXML moduleInput host XML node
#'
#' @return \code{moduleInputHost} object
#'
#' @seealso \code{readModuleHostXML}
#'
#' @import XML
readModuleInputHostXML <- function (moduleInputHostXML) {
    if (xmlName(moduleInputHostXML) != "moduleInput")
        stop("moduleInput element required")
    attrs <- xmlAttrs(moduleInputHostXML)
    name <- attrs[["name"]]
    moduleInputHost(name = name)
}

#' @describeIn moduleHostToXML Create XML corresponding to a
#'     \code{moduleInputHost} object
#'
#' @import XML
#' @export
moduleHostToXML.moduleInputHost <- function(moduleInputHost) {
    child <- newXMLNode("moduleInput", attrs = moduleInputHost)
    newXMLNode(name = "host", kids = list(child))
}
