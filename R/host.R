#' \code{moduleHost} object
#'
#' @seealso \code{vagrantHost}, \code{module}
#'
#' @name moduleHost
NULL

#' Create a \code{vagrantHost} object
#'
#' Create a \code{moduleHost} object of class \code{vagrantHost} to
#' describe a vagrant machine for executing module source scripts.
#'
#' The default setup for vagrant synced folders is for the directory
#' where the Vagrantfile is found to be synced to \file{/vagrant} on
#' the vagrant guest. If the vagtant machine describes a different
#' synced folder setup the synced folder on the host machine can be
#' specified with \code{hostdir}, and on the vagrant guest with
#' \code{guestdir}.
#'
#' @param vagrantfile Path to vagrantfile
#' @param hostdir Host synced folder path
#' @param guestdir Gues synced folder path
#'
#' @return Object of class \code{vagrantHost} and \code{moduleHost}
#'
#' @export
vagrantHost <- function(vagrantfile, hostdir = dirname(vagrantfile),
                        guestdir = "/vagrant") {
    if (!file.exists(vagrantfile))
        stop("vagrantfile does not exist")
    if (!dir.exists(hostdir))
        stop("hostdir does not exist")
    vagrantfile <- normalizePath(vagrantfile)
    hostdir <- normalizePath(hostdir)
    vagrantHost <- list(vagrantfile = vagrantfile,
                        synhost = hostdir,
                        guestdir = guestdir)
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

#' Create XML corresponding to a \code{vagrantHost} object
moduleHostToXML.vagrantHost <- function(vagrantHost) {
    if (!inherits(vagrantHost, "vagrantHost"))
        stop ("vagrantHost object required")
    newXMLNode("vagrant", attrs = vagrantHost)
}
