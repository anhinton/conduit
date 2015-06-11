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
#'
#' @details Creates a .py script file from the supplied \code{module},
#' taking specific input file paths from \code{inputs}.
#'
#' Writes script to \code{modulePath}, then attempts to execute the
#' script in this location.
#'
#' @param module \code{module} object
#' @param inputObjects Named list of input objects
#' 
#' @return named list of \code{moduleOutput} objects
executeScript.python <- function(module, inputObjects) {
    language <- "python"
    internalExtension <- ".pickle"
    
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
    moduleScript <- c("import os", "import pickle",
                      inputScript, sourceScript, outputScript)

    ## write script file to disk
    scriptPath <- "script.python"
    scriptFile <- file(scriptPath)
    writeLines(moduleScript, scriptFile)
    close(scriptFile)

    ## batch the script file in an python session
    systemCall <-
        switch(Sys.info()["sysname"],
               Linux = "/usr/bin/python",
               stop("conduit does not support python on your system"))
    arguments <- c(scriptPath)
    try(system2(systemCall, arguments))

    objects <- lapply(outputs, checkOutputObject, language, getwd())
    return(objects)
}
