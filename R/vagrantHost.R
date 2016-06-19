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
#' @export
moduleHostToXML.vagrantHost <- function(moduleHost) {
    vagrantHost <- moduleHost
    child <- newXMLNode("vagrant", attrs = vagrantHost)
    newXMLNode(name = "host", kids = list(child))
}

#' @describeIn prepareModuleHost prepare \code{vagrantHost}; returns
#' path to unique module output directory relative to hostdir/guestdir
#'
#' @export
prepareModuleHost.vagrantHost <- function(moduleHost, moduleName,
                                          modulePath) {
    vagrantHost <- moduleHost

    ## create unique module output directory--'outputLocation'--in
    ## 'hostdir' on local machine
    hostdir <- vagrantHost$hostdir
    outputLocation <- tempfile(pattern = moduleName,
                               tmpdir = file.path("conduit.out"))
    hostdir <- file.path(hostdir, outputLocation)
    if (dir.exists(hostdir))
        unlink(hostdir, recursive = TRUE)
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
#'
#' @export
executeCommand.vagrantHost <- function(moduleHost, outputLocation,
                                       command) {
    commanddir <- dirname(moduleHost$vagrantfile)
    oldwd <- setwd(commanddir)
    on.exit(setwd(oldwd))
    args <- c(command$command, command$args)
    guestdir <- file.path(moduleHost$guestdir, outputLocation)
    args <- paste("ssh", "-c", "'cd", guestdir, ";",
                  paste(args, collapse = " "), "'")
    system2(command = "vagrant", args = args, stdout = TRUE, stderr = TRUE)
}

#' @describeIn retrieveModuleHost retrieve module output from
#'     \code{vagrantHost}
#'
#' @export
retrieveModuleHost.vagrantHost <- function(moduleHost, outputLocation,
                                           modulePath) {
    hostdir <- file.path(moduleHost$hostdir, outputLocation)
    files <- list.files(path = hostdir, all.files = TRUE, full.names = TRUE)
    ## we return all.files = TRUE to capture hidden files, but as a result we
    ## need to exclude current (.)  and parent (..) directory from files to
    ## be copied
    files <- files[basename(files) != ".." & basename(files) != "."]
    for (f in files)
        file.copy(f, modulePath, recursive = TRUE)
}
