### Platform support for shell platform

#' prepare internal input script for shell language
internalInputScript.shell <- function (symbol, inputObject) {
    script <- paste0(symbol, "=$(cat ", inputObject, ")")
    return(script)
}

#' create script to create internal output for language = "shell"
internalOutputScript.shell <- function (symbol) {
    script <- paste0("echo ${", symbol, "} > ", symbol, ".txt")
    return(script)
}

#' Execute a script in the "shell" language
#'
#' @details Creates a .sh script file from the supplied \code{module},
#' taking specific input file paths from \code{inputs}.
#'
#' Writes script to \code{modulePath}, then attempts to execute the
#' script in this location.
#'
#' @param module \code{module} object
#' @param inputObjects Named list of input objects
#' 
#' @return named list of \code{moduleOutput} objects
executeScript.shell <- function(module, inputObjects) {
    language <- "shell"
    internalExtension <- ".txt"
    
    ## sort sources into correct order
    sources <- module$sources
    sources <- lapply(sourceOrder(sources),
                      function (x, sources) {
                          sources[[x]]
                      }, sources)

    ## sourceScript contains the module's source(s) to be evaluated
    sourceScript <-
        lapply(
            sources,
            function (moduleSource) {
                class(moduleSource) <- class(moduleSource$vessel)
                script <- extractModuleSource(moduleSource)
                return(script)
            })
    sourceScript <- unlist(sourceScript, use.names = FALSE)

    ## inputScript loads the module's designated inputs
    inputs <- module$inputs
    inputScript <-
        lapply(
            inputs,
            function (input, inputObjects, language) {
                resource <- getElement(inputObjects, input$name)
                script <- ensureModuleInput(input, resource, language)
                return(script)
            }, inputObjects, language)
    inputScript <- unlist(inputScript, use.names = FALSE)

    ## outputScript loads the module's designated outputs
    outputs <- module$outputs
    outputScript <-
        lapply(outputs, ensureModuleOutput, language)
    outputScript <- unlist(outputScript, use.names = FALSE)

    ## moduleScript combines the scripts in correct order
    moduleScript <- c(inputScript, sourceScript, outputScript)

    ## write script file to disk
    scriptPath <- "script.sh"
    scriptFile <- file(scriptPath)
    writeLines(moduleScript, scriptFile)
    close(scriptFile)

    ## batch the script file in a shell session
    systemCall <-
        switch(Sys.info()["sysname"],
               Linux = "/bin/bash",
               stop("conduit does not support R on your system"))
    arguments <- c(scriptPath)
    try(system2(systemCall, arguments))

    objects <- lapply(outputs, checkOutputObject, internalExtension)
    return(objects)
}
