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
executeScript.R <- function(script) {
    ## batch the script file in an R session
    systemCall <-
        switch(Sys.info()["sysname"],
               Linux = "Rscript",
               stop("conduit does not support R on your system"))
    scriptPath <- script$scriptPath
    host <- script$host
    if (is.null(host)) {
        system2(systemCall, scriptPath)
    } else {
        system2(
            "ssh",
            c("-i", defaultIdfile,
              "-p", host$port,
              paste0(host$user, "@", host$address),
              paste("'cd", dirname(scriptPath), ";",
                    systemCall, basename(scriptPath), "'")))
    }
}
