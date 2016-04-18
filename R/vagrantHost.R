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
#' @param guestdir Guest synced folder path
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
    if (!is_length1_char(guestdir))
        stop("guestdir must be a length one character string")
    vagrantfile <- normalizePath(vagrantfile)
    hostdir <- normalizePath(hostdir)
    vagrantHost <- list(vagrantfile = vagrantfile,
                        hostdir = hostdir,
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
    hostdir <-
        if ("hostdir" %in% names(attrs)) {
            attrs[["hostdir"]]
        } else {
            dirname(vagrantfile)
        }
    guestdir <-
        if ("guestdir" %in% names(attrs)) {
            attrs[["guestdir"]]
        } else {
            "/vagrant"
        }
    vagrantHost(vagrantfile = vagrantfile, hostdir = hostdir,
                guestdir = guestdir)
}

#' @describeIn moduleHostToXML Create XML corresponding to a
#'     \code{vagrantHost} object
#'
#' @import XML
moduleHostToXML.vagrantHost <- function(vagrantHost) {
    if (!inherits(vagrantHost, "vagrantHost"))
        stop ("vagrantHost object required")
    child <- newXMLNode("vagrant", attrs = vagrantHost)
    newXMLNode(name = "host", kids = list(child))
}

#' @describeIn prepareModuleHost prepare \code{vagrantHost}; returns
#' path to unique module output directory relative to hostdir/guestdir
prepareModuleHost.vagrantHost <- function(moduleHost, moduleName,
                                          modulePath) {
    if (!inherits(moduleHost, "vagrantHost"))
        stop("vagrantHost object required")
    if (!is_length1_char(moduleName))
        stop("moduleName is not length 1 character")
    if (!dir.exists(modulePath))
        stop("modulePath does not exist")

    vagrantHost <- moduleHost

    ## create unique module output directory--'outputLocation'--in
    ## 'hostdir' on local machine
    hostdir <- vagrantHost$hostdir
    outputLocation <- tempfile(pattern = moduleName,
                               tmpdir = file.path("conduit.out"))
    hostdir <- file.path(hostdir, outputLocation)
    if (dir.exists(hostdir))
        unlink(hostdir, rescursive = TRUE)
    dir.create(hostdir, recursive = TRUE)
    
    ## make contents of modulePath available to vagrantHost
    files <- list.files(path = modulePath, full.names = TRUE)
    for (f in files)
        file.copy(f, hostdir, recursive = TRUE)

    ## return outputLocation object
    class(outputLocation) <- c("vagrantHostOutputLocation", "outputLocation")
    outputLocation
}

#' @describeIn executeCommand execute command on a \code{vagrantHost}
executeCommand.vagrantHost <- function(moduleHost, outputLocation,
                                       command) {
    commanddir <- dirname(moduleHost$vagrantfile)
    oldwd <- setwd(commanddir)
    on.exit(setwd(oldwd))
    args <- c(command$command, command$args)
    guestdir <- file.path(moduleHost$guestdir, moduleHostSubdir)
    args <- paste("ssh", "-c", "'cd", guestdir, ";",
                  paste(args, collapse = " "), "'")
    system2(command = "vagrant",
            args = args)
}

#' @describeIn retrieveModuleHost retrieve module output from
#'     \code{vagrantHost}
retrieveModuleHost.vagrantHost <- function(moduleHost, hostSubdir, modulePath) {
    hostdir <- file.path(moduleHost$hostdir, hostSubdir)
    files <- list.files(path = hostdir, full.names = TRUE)
    for (f in files)
        file.copy(f, modulePath, recursive = TRUE)
}
