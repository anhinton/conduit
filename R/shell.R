### Platform support for shell platform

#' @describeIn internalInputScript prepare internal input script for
#' shell language
internalInputScript.shell <- function (symbol, inputObject) {
    script <- paste0(symbol, "=$(cat ", inputObject, ")")
    return(script)
}

#' @describeIn internalOutputScript create script to create internal
#' output for language = "shell"
internalOutputScript.shell <- function (symbol) {
    script <- paste0("echo ${", symbol, "} > ", symbol, ".txt")
    return(script)
}

#' @describeIn executeScript Execute a script in the "shell" language
executeScript.shell <- function(script, host) {
    ## batch the script file in a shell session
    if (is.null(host)) {
        systemCall <-
            switch(Sys.info()["sysname"],
                   Linux = "sh",
                   stop("conduit does not support shell on your system"))
        system2(systemCall, script)
    } else {
        user <- host$user
        address <- host$address
        port <- host$port
        directory <- host$directory
        idfile <- host$idfile
        exec_result <- system2(
            "ssh",
            c("-i", idfile,
              "-p", port,
              paste0(user, "@", address),
              paste("'cd", directory, ";",
                    "sh", script, "'")))
        return(exec_result)
    }
}
