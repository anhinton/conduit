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
executeScript.shell <- function(script) {
    ## batch the script file in a shell session
    systemCall <-
        switch(Sys.info()["sysname"],
               Linux = "bash",
               stop("conduit does not support shell on your system"))
    arguments <- c(script)
    system2(systemCall, arguments)
}
