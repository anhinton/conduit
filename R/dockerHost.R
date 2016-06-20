#' Create a \code{dockerHost} object
#'
#' Create a \code{moduleHost} object of class \code{dockerHost} to
#' describe a docker image for executing module source scripts.
#'
#' The default Docker setup for docker synced folders is that no directories
#' are shared.  
#' The default 'conduit' setup for docker synced folders is that the
#' directory that the module is run within is shared with \file{/home/conduit}
#' on the guest container.
#' The synced folder location on the guest container can be customised
#' with \code{guestdir}
#'
#' @param image Name of Docker image
#' @param guestdir Guest synced folder path
#'
#' @return Object of class \code{dockerHost} and \code{moduleHost}
#'
#' @export
dockerHost <- function(image, guestdir = "/home/conduit") {
    if (!is_length1_char(image))
        stop("image must be a length one character string")
    if (!is_length1_char(guestdir))
        stop("guestdir must be a length one character string")
    dockerHost <- list(image = image,
                       guestdir = guestdir)
    class(dockerHost) <- c("dockerHost", "moduleHost")
    dockerHost
}

#' Create a \code{dockerHost} object from <host><docker/></host> XML
#'
#' @param dockerHostXML docker host XML node
#'
#' @return \code{dockerHost} object
#'
#' @seealso \code{readModuleHostXML}
#'
#' @import XML
readDockerHostXML <- function (dockerHostXML) {
    if (xmlName(dockerHostXML) != "docker")
        stop("docker element required")
    attrs <- xmlAttrs(dockerHostXML)
    image <- attrs[["image"]]
    guestdir <-
        if ("guestdir" %in% names(attrs)) {
            attrs[["guestdir"]]
        } else {
            "/home/conduit"
        }
    dockerHost(image = image,
               guestdir = guestdir)
}

#' @describeIn moduleHostToXML Create XML corresponding to a
#'     \code{dockerHost} object
#'
#' @import XML
#' @export
moduleHostToXML.dockerHost <- function(moduleHost) {
    dockerHost <- moduleHost
    child <- newXMLNode("docker", attrs = dockerHost)
    newXMLNode(name = "host", kids = list(child))
}

#' @describeIn prepareModuleHost prepare \code{dockerHost}. Returns
#'     empty string.
#'
#' @export
prepareModuleHost.dockerHost <- function(moduleHost, moduleName, modulePath) {
    ## module execution directory is shared with guest container
    ## SO nothing to do
    ## EXCEPT return an empty string as 'hostSubdir' to satisfy
    ## the 'conduit' template for these generics
    outputLocation <- ""

    ## return outputLocation object
    class(outputLocation) <- c("dockerHostOutputLocation", "outputLocation")
    outputLocation
}

#' @describeIn executeCommand execute command on a \code{dockerHost}
#'
#' @export
executeCommand.dockerHost <- function(moduleHost, outputLocation, command) {
    args <- c(command$command, command$args)
    hostdir <- getwd()
    guestdir <- moduleHost$guestdir
    args <- paste("run", 
                  "-v", paste0(hostdir, ":", guestdir),
                  "-w", guestdir, 
                  moduleHost$image,
                  paste(args, collapse = " "))
    system2(command = "docker", args = args, stdout = TRUE, stderr = TRUE)
}

#' @describeIn retrieveModuleHost does nothing
#'
#' @export
retrieveModuleHost.dockerHost <- function(moduleHost, outputLocation,
                                          modulePath) {
    # module execution directory is shared with guest container
    # SO nothing to do
}
