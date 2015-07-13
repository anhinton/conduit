### R language support

#' @describeIn internalInputScript prepare internal input script for R
#' language
internalInputScript.R <- function (symbol, inputObject) {
    script <- paste0(symbol, " <- readRDS(\"", inputObject, "\")")
    return(script)
}

#' @describeIn internalOutputScript create script to create internal
#' output for language = "R"
internalOutputScript.R <- function (symbol) {
    script <- paste0("saveRDS(", symbol, ", file = \"", symbol, ".rds\")")
    return(script)
}

#' @describeIn executeScript Execute a script in the "R" language
executeScript.R <- function(scriptPath, host) {
    ## batch the script file in an R session
    systemCall <-
        switch(Sys.info()["sysname"],
               Linux = "Rscript",
               stop("conduit does not support R on your system"))
    if (is.null(host)) {
        system2(systemCall, scriptPath)
    } else {
        user <- host$user
        address <- host$address
        port <- host$port
        directory <- host$dir
        exec_result <- system2(
            "ssh",
            c("-i", defaultIdfile,
              "-p", port,
              paste0(user, "@", address),
              paste("'cd", directory, ";",
                    systemCall, basename(scriptPath), "'")))
        return(exec_result)
    }
}
