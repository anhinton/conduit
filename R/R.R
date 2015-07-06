### R language support

#' prepare internal input script for R language
internalInputScript.R <- function (symbol, inputObject) {
    script <- paste0(symbol, " <- readRDS(\"", inputObject, "\")")
    return(script)
}

#' create script to create internal output for language = "R"
internalOutputScript.R <- function (symbol) {
    script <- paste0("saveRDS(", symbol, ", file = \"", symbol, ".rds\")")
    return(script)
}

#' Execute a script in the "R" language
executeScript.R <- function(script) {
    ## batch the script file in an R session
    systemCall <-
        switch(Sys.info()["sysname"],
               Linux = "Rscript",
               stop("conduit does not support R on your system"))
    arguments <- c(script)
    system2(systemCall, arguments)
}
