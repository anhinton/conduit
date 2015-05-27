#' Execute a script in the "R" language
#'
#' @details Creates a .R script file from the supplied \code{module},
#' taking specific input file paths from \code{inputs}.
#'
#' Writes script to \code{modulePath}, then attempts to execute the
#' script in this location.
#'
#' @param module \code{module} object
#' @param resources Named list of input objects
#' 
#' @return FIXME: nothing meaningful
executeScript.R <- function(module, resources) {
    language <- "R"
    
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
            function (input, resources, language) {
                resource <- getElement(resources, input$name)
                script <- ensureModuleInput(input, resource, language)
                return(script)
            }, resources, language)
    inputScript <- unlist(inputScript, use.names = FALSE)

    ## outputScript loads the module's designated outputs
    outputs <- module$outputs
    outputScript <-
        lapply(outputs, ensureModuleOutput, language)
    outputScript <- unlist(outputScript, use.names = FALSE)

    ## moduleScript combines the scripts in correct order
    moduleScript <- c(inputScript, sourceScript, outputScript)

    ## write script file to disk
    scriptName <- "script.R"
    scriptFile <- file(scriptName)
    writeLines(moduleScript, scriptFile)
    close(scriptFile)

    ## batch the script file in an R session
    systemCall <- paste0("Rscript \"", scriptPath, "\"")
    systemCall <-
        switch(Sys.info()["sysname"],
               Linux = "/usr/bin/Rscript",
               stop("conduit does not support R on your system"))
    arguments <- c(scriptName)
    try(system2(systemCall, arguments))
}
