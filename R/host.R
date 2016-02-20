#' Constructor functions for \code{moduleHost} objects
#'
#' These functions are used to create \code{moduleHost} objects.
#'
#' @name moduleHost
#'
#' @param vagrantfile Path to vagrantfile
#' @param location File location for running vagrant machine
#'
#' @return Object of class \code{moduleHost}, and one of:
#'
#' \item{\code{vagrantHost}}{containing \code{vagrantfile} and
#'     \code{location}}
NULL

#' @describeIn moduleHost Create a \code{vagrantHost} object
#'
#' @export
vagrantHost <- function(vagrantfile, location = NULL) {
    if (!file.exists(vagrantfile))
        stop("vagrantfile does not exist")
    if (is.null(location))
        location <- dirname(vagrantfile)
    if (!dir.exists(location))
        stop("location does not exist")
    vagrantfile <- normalizePath(vagrantfile)
    location <- normalizePath(location)
    vagrantHost <- list(vagrantfile = vagrantfile,
                        location = location)
    class(vagrantHost) <- c("vagrantHost", "moduleHost")
    vagrantHost
}

#' Create a \code{vagrantHost} object from <host><vagrant/></host> XML
#'
#' @param vagrantHostXML vagrant host XML node
#'
#' @return \code{vagrantHost} object
#'
#' @seealso \code{readModuleHostXML}
#'
#' @import XML
readVagrantHostXML <- function (vagrantHostXML) {
    if (xmlName(vagrantHostXML) != "vagrant")
        stop("vagrant element required")
    attrs <- xmlAttrs(vagrantHostXML)
    vagrantfile <- attrs[["vagrantfile"]]
    location <-
        if ("location" %in% names(attrs)) {
            attrs[["location"]]
        } else {
            NULL
        }
    vagrantHost(vagrantfile, location)
}
