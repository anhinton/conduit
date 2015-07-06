### Platform support for python platform

#' prepare internal input script for python language
internalInputScript.python <- function (symbol, inputObject) {
    script <- c(paste0("with open('", inputObject,
                       "', 'rb') as f:"),
                paste0("\t", symbol, " = pickle.load(f)"))
    return(script)
}

#' create script to create internal output for language = "python"
internalOutputScript.python <- function (symbol) {
    script <- c(paste0("with open('", symbol, ".pickle', 'wb') as f:"),
                paste0("\tpickle.dump(", symbol, ", f)"))
    return(script)
}

#' Execute a script in the "python" language
executeScript.python <- function(script) {
    ## batch the script file in a python session
    systemCall <-
        switch(Sys.info()["sysname"],
               Linux = "python",
               stop("conduit does not support python on your system"))
    arguments <- c(script)
    system2(systemCall, arguments)
}
