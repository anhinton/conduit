initIdfile <- function () {
    Sys.chmod(system.file("conduit.key", package = "conduit"), mode = "0400")
    assign("defaultIdfile",
           system.file("conduit.key", package = "conduit"),
           .conduit.global)
}

#' Creates module output directory on host
#'
#' @details Directory to be created is given by \code{host$directory}
#'
#' @param host remote host list
#'
#' @seealso \code{runModule}
#'
#' @return 0 if success
createHostDirectory <- function(host) {
    user <- host$user
    address <- host$address
    port <- host$port
    directory <- host$directory
    idfile <- host$idfile
    args <- c("-i", idfile,
              "-p", port,
              paste0(user, "@", address),
              paste("'mkdir -p", directory, "'"))
    result <- system2("ssh", args)
    return(result)
}

#' Copy a file to remote module host
#'
#' @details copies file at \code{file} to remote directory
#' \code{host$directory}.
#'
#' @param file file to copy
#' @param host host list
#' @param idfile login credentials
#'
#' @seealso \code{runModule}
#'
#' @return 0 if successful
fileToHost <- function(file, host,
                       idfile = get("defaultIdfile", envir = .conduit.global)) {
    user <- host$user
    address <- host$address
    port <- host$port
    directory <- host$directory
    
    args <- c("-i", idfile,
              "-P", port,
              file,
              paste0(user, "@", address, ":", directory))
    result <- system2("scp", args)
    return(result)
}

#' Fetch file from remote host.
#'
#' @details copies file at \code{file} from remote directory
#' \code{host$directory} to the current working directory.
#'
#' @param file file path
#' @param host host list
#'
#' @return 0 if successful
fetchFromHost <- function(file, host) {
    user <- host$user
    address <- host$address
    port <- host$port
    directory <- host$directory
    idfile <- host$idfile

    args <- c("-i", idfile,
              "-P", port,
              paste0(user, "@", address, ":",
                     file.path(directory, file, fsep = "/")),
              ".")
    result <- system2("scp", args)
    return(result)
}
