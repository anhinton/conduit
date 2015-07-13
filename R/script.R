#' @describeIn extractModuleSource Extract a module's source script
#' from a scriptVessel
extractModuleSource.scriptVessel <- function(moduleSource) {
    script <- moduleSource$vessel$value
    return(script)
}

#' @describeIn extractModuleSource Extract a module's source script
#' from a fileVessel
extractModuleSource.fileVessel <- function(moduleSource) {
    script <- readLines(moduleSource$vessel$ref)
    return(script)
}

#' Extract a module's source script
#'
#' @param vessel Vessel indicating source script location
#'
#' @return character vector of source scrip
#'
#' @seealso \code{executeScript}
extractModuleSource <- function(moduleSource) {
    UseMethod("extractModuleSource")
}

#' Checks a module output object has been created.
#'
#' @details Will produce an error if the object does not exist.
#'
#' @param output \code{moduleOutput} object
#' @param internalExtension file extension for serialized internal language
#' object
#'
#' @return named list containing:
#' \itemize{
#'   \item name: object name
#'   \item type: object vessel type
#'   \item object: output object
#' }
checkOutputObject <- function (output, language, outputDirectory = getwd()) {
    name <- output$name
    vessel <- output$vessel
    type <- class(vessel)[[1]]
    object <- outputObject(output, language, outputDirectory)
    object <- try(normalizePath(object))

    if (type == "internalVessel" || type == "fileVessel") {
        if (!file.exists(object)) {
            stop(paste0("output object '", name, "' does not exist"))
        }
    }
    object <- list(name = name, type = type, object = object)
    return(object)
}

#' Determines running order for \code{moduleSource}s.
#'
#' @details Order goes negative < 0 < no order given < positive.
#'
#' @param sources List of \code{moduleSource}s
#' 
#' @return Running order as numeric vector
#' 
#' @seealso \code{moduleSource}
sourceOrder <- function(sources) {
    ## extract order values from sources
    orderValues <- sapply(sources,
                          function(x) {
                              value <-
                                  if (is.null(x$order)) {
                                      NA
                                  } else {                              
                                      as.numeric(x$order)
                                  }
                              return(value)
                          })
    ## logical vector of which order values <= 0
    zeroLess <- !is.na(orderValues) & orderValues <= 0
    ## numeric ordering of above
    zeroLessOrder <- order(orderValues[zeroLess])
    ## indices of order values <=0 ordered by zeroLessOrder
    zeroLessOrdered <- which(zeroLess)[zeroLessOrder]
    ## pos: values > 0 ordered
    pos <- !is.na(orderValues) & orderValues > 0
    posOrder <- order(orderValues[pos])
    ## indices of order values > 0 ordered by posOrder
    posOrdered <- which(pos)[posOrder]
    ## indices of missing order values
    unorderedOrdered <- which(is.na(orderValues))
    ## negative < 0 < unordered < positive
    c(zeroLessOrdered, unorderedOrdered, posOrdered)
}

#' prepare script to resolve internal input
#'
#' @param symbol character string with class set to language of module script
#' @param inputObject file location of serialised language object
#'
#' @return character vector of script to ensure input
internalInputScript <- function(symbol, inputObject) {
    UseMethod("internalInputScript", object = symbol)
}

#' prepare script to resolve internal output
#'
#' @param symbol character string with class set to language of module script
#'
#' @return character vector of script to ensure input
internalOutputScript <- function (symbol) {
    UseMethod("internalOutputScript")
}

#' Prepare script to create inputs
#'
#' @param input input name
#' @param inputObject object to be supplied as input
#' @param language module language
#'
#' @return Script as character vector
prepareScriptInput <- function(input, inputObject, language) {
    type <- class(input$vessel)[1]
    script <- switch(
        type,
        internalVessel = {
            symbol <- input$vessel$symbol
            class(symbol) <- language
            internalInputScript(symbol, inputObject)
        },
        NULL)
    return(script)
}

#' Prepare script to create outputs
#'
#' @param output output name
#' @param language module language
#'
#' @return Script as character vector
prepareScriptOutput <- function(output, language) {
    type <- class(output$vessel)[1]
    script <- switch(
        type,
        internalVessel = {
            symbol <- output$vessel$symbol
            class(symbol) <- language
            internalOutputScript(symbol)
        },
        NULL)
    return(script)
}

#' Default idfile for ssh to remote hosts
defaultIdfile <- system.file("conduit.key", package = "conduit")    

#' Prepare a script for executing a module in its language.
#'
#' @details Resolves the module's internal inputs and creates a script
#' file from the supplied \code{module},
#'
#' @param module \code{module} object
#' @param inputObjects Named list of input objects
#' 
#' @return List object containg scriptPath and host, with class set to
#' module$language.
prepareScript <- function(module, inputObjects) {
    language <- module$language

    host <- module$host
    host <-
        if (is.null(host)) {
            NULL
        } else {
            parseModuleHost(host)
        }
    
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
                inputObject <- getElement(inputObjects, input$name)
                script <- prepareScriptInput(input, inputObject, language)
                return(script)
            }, inputObjects, language)
    inputScript <- unlist(inputScript, use.names = FALSE)

    ## outputScript loads the module's designated outputs
    outputs <- module$outputs
    outputScript <-
        lapply(outputs, prepareScriptOutput, language)
    outputScript <- unlist(outputScript, use.names = FALSE)

    ## moduleScript combines the scripts in correct order
    moduleScript <- switch(
        language,
        python = c("import os", "import pickle",
            inputScript, sourceScript, outputScript),
        c(inputScript, sourceScript, outputScript))
    
    ## write script file to disk
    
    scriptPath <- paste0("script", scriptExtension(language))
    scriptFile <- file(scriptPath)
    writeLines(moduleScript, scriptFile)
    close(scriptFile)

    if (!is.null(host)) {
        idfile <- defaultIdfile
        user <- host$user
        address <- host$address
        port <- host$port
        remotePath <- file.path(tempfile(pattern = "module"), scriptPath)
        ## create module directory on remote machine
        system2("ssh", c("-i", idfile, "-p", port, paste0(user, "@", address),
                         paste0("'mkdir -p ", dirname(remotePath), "'")))
        ## copy script to module directory
        args <- c("-i", idfile,
                  "-P", port,
                  scriptPath,
                  paste0(user, "@", address, ":", remotePath))
        system2("scp", args)
        scriptPath <- remotePath
    }
    script <- list(scriptPath = scriptPath, host = host)
    class(script) <- module$language
    return(script)
}

#' Execute a prepared module script file.
#'
#' @param script script file to be executed
#'
#' @seealso \code{runModule}
#' 
#' @return named list of \code{moduleOutput} objects
executeScript <- function(script) {
    UseMethod("executeScript")
}
