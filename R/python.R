### Platform support for python platform

#' @describeIn internalInputScript prepare internal input script for
#' python language
internalInputScript.python <- function (symbol, inputObject) {
    script <- c(paste0("with open('", inputObject,
                       "', 'rb') as f:"),
                paste0("\t", symbol, " = pickle.load(f)"))
    return(script)
}

#' @describeIn internalOutputScript create script to create internal
#' output for language = "python"
internalOutputScript.python <- function (symbol) {
    script <- c(paste0("with open('", symbol, ".pickle', 'wb') as f:"),
                paste0("\tpickle.dump(", symbol, ", f)"))
    return(script)
}

#' @describeIn executeScript Execute a script in the "python" language
executeScript.python <- function(script, host) {    
    ## batch the script file in a python session
    if (is.null(host)) {
        systemCall <-
            switch(Sys.info()["sysname"],
                   Linux = "python",
                   stop("conduit does not support python on your system"))    
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
                    "python", script, "'")))
        return(exec_result)
    }
}
