#' @describeIn extractModuleSource Extract a module's source script
#'     from a scriptVessel
#'
#' @export
extractModuleSource.scriptVessel <- function(moduleSource) {
    script <- moduleSource$vessel$value
    return(script)
}

#' @describeIn extractModuleSource Extract a module's source script
#' from a fileVessel
#'
#' @export
extractModuleSource.fileVessel <- function(moduleSource) {
    vessel <- moduleSource$vessel
    ref <- vessel$ref
    path <- vessel$path
    location <- moduleSource$location
    file <- findFile(ref, path, location)
    script <- readLines(file)
    return(script)
}

#' @describeIn extractModuleSource Extract a module's source script
#' from a urlVessel
#'
#' @export
extractModuleSource.urlVessel <- function(moduleSource) {
    con <- url(ref)
    on.exit(close(con))
    script <- readLines(con)
    return(script)
}

#' Extract a module's source script
#'
#' @param moduleSource source slot of \code{module} object
#'
#' @return character vector of source script
#'
#' @seealso \code{executeScript}
#'
#' @export
extractModuleSource <- function(moduleSource) {
    UseMethod("extractModuleSource")
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

#' Prepare a script for executing a module in its language.
#'
#' This function creates an executable script file from a
#' \code{module} object.
#'
#' The script returned will include code to load internal inputs,
#' followed by the module source scripts in the correct order, and
#' ending with code to produce internal outputs for consumption by
#' other modules.
#'
#' The resulting script is saved to the current working directory.
#'
#' @param module \code{module} object
#' 
#' @return \code{script} object naming script file
#'
#' @seealso Called by \code{runModule}. \code{module}
prepareScript <- function(module) {
    if (!inherits(module, "module"))
        stop("module object required")
    language <- getLanguage(module)
    location <- attr(module, "location")

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
            function (moduleSource, location) {
                class(moduleSource) <- class(moduleSource$vessel)
                moduleSource$location <- location
                script <- extractModuleSource(moduleSource)
                return(script)
            }, location)
    sourceScript <- unlist(sourceScript, use.names = FALSE)

    ## inputScript loads the module's designated inputs
    inputs <- module$inputs
    inputScript <- lapply(inputs, prepareScriptInput, language)
    inputScript <- unlist(inputScript, use.names = FALSE)

    ## outputScript loads the module's designated outputs
    outputs <- module$outputs
    outputScript <-
        lapply(outputs, prepareScriptOutput, language)
    outputScript <- unlist(outputScript, use.names = FALSE)

    moduleScript <- c(inputScript, sourceScript, outputScript)
    moduleScript <- switch(
        language,
        python = c("#!/usr/bin/python", "import os", "import pickle",
                   moduleScript),
        R = c("#!/usr/bin/Rscript", moduleScript),
        shell = c("#!/bin/sh", moduleScript))
    ## script might be empty
    if (is.null(moduleScript))
        moduleScript <- ""

    ## write script file to disk

    scriptPath <- paste0("script", scriptExtension(language))
    scriptFile <- file(scriptPath)
    writeLines(moduleScript, scriptFile)
    close(scriptFile)

    class(scriptPath) <- c(paste0(language, "Script"), "script")
    scriptPath
}

#' Prepare script to create inputs
#'
#' @details if a module input is to be fulfilled via an internalVessel
#'     the module source scripts will require the symbol to be loaded
#'     prior to execution. other vessel types do not need to be loaded
#'     in script.
#' 
#' @param moduleInput module input object
#' @param language module language
#'
#' @return Script as character vector
prepareScriptInput <- function(moduleInput, language) {
    if (!inherits(moduleInput, "moduleInput"))
        stop("moduleInput object required")
    vessel <- getVessel(moduleInput)
    if (inherits(vessel, "internalVessel")) {
        symbol <- vessel$symbol
        class(symbol) <- c(paste0(language, "Symbol"), class(symbol))
        internalInputScript(symbol)
    } else {
        NULL
    }
}

#' Prepare script to create outputs
#'
#' @details if a module output is passed to conduit via an
#'     internalVessel the module source scripts must serialize the
#'     object after execution. other vessel types do not need this to
#'     be done by the glue system.
#'
#' @param moduleOutput \code{moduleOutput} object
#' @param language module language
#'
#' @return Script as character vector
prepareScriptOutput <- function(moduleOutput, language) {
    if (!inherits(moduleOutput, "moduleOutput"))
        stop("moduleOutput object required")
    vessel <- getVessel(moduleOutput)
    if (inherits(vessel, "internalVessel")) {
        symbol <- vessel$symbol
        class(symbol) <- c(paste0(language, "Symbol"), class(symbol))
        internalOutputScript(symbol)
    } else {
        NULL
    }
}

#' Prepare script for internal inputs
#'
#' These functions prepare a module script snippet to resolve an
#' internal input
#'
#' @param symbol \code{symbol} object
#'
#' @return script as character vector
#'
#' @name internalInputScript
internalInputScript <- function(symbol) {
    if (!inherits(symbol, "symbol"))
        stop("symbol object required")
    UseMethod("internalInputScript")
}

#' prepare script to resolve internal output
#'
#' These functions prepare a module script snippet to produce an
#' internal output
#'
#' @param symbol \code{symbol} object
#'
#' @return script as character vector
internalOutputScript <- function (symbol) {
    if (!inherits(symbol, "symbol"))
        stop("symbol object required")
    UseMethod("internalOutputScript")
}

#' Execute a prepared module script file.
#'
#' @details If \code{moduleHost} is provided script will be executed on
#' remote host in \code{outputLocation} on that machine.
#'
#' \code{outputLocation} should be the result of running
#' \code{prepareModuleHost}
#'
#' @seealso \code{moduleHost}, \code{prepareModuleHost}
#'
#' @param script \code{script} object to be executed
#' @param moduleHost \code{moduleHost} object
#' @param outputLocation \code{outputLocation} object
#'
#' @seealso \code{runModule}
#' 
#' @return 0 if successful
executeScript <- function(script, moduleHost, outputLocation) {
    if (!inherits(script, "script"))
        stop("script object required")
    if (!inherits(moduleHost, "moduleHost") && !is.null(moduleHost))
        stop("moduleHost object required")
    if (!inherits(outputLocation, "outputLocation") && !is.null(outputLocation))
        stop("outputLocation object required")
    command <- command(script)
    executeCommand(moduleHost, outputLocation, command)
}

#' Generate a system command to run a module's source scripts
#'
#' @details \code{script} should be the result of \code{prepareScript}
#'
#' This function is usually called by \code{executeScript}.
#'
#' @param \code{script} object
#'
#' @return \code{command} list containing \code{command} and
#'     \code{args} character vectors
#'
#' @seealso \code{prepareScript}, \code{executeScript}
command <- function(script) {
    if (!inherits(script, "script"))
        stop("script object required")
    UseMethod("command")
}

#' Execute a \code{command} list object
#'
#' These methods execute a command list prepared by the \code{command}
#' function.
#'
#' If a \code{moduleHost} is provided the command is executed in the
#' \code{outputLocation} on the host machine.
#'
#' This function is usually called by \code{executeScript}.
#'
#' @param moduleHost \code{moduleHost} object
#' @param outputLocation \code{outputLocation} object
#' @param command \code{command} object
#'
#' @seealso This function called by
#'     \code{executeScript}. \code{moduleHost},
#'     \code{prepareModuleHost} for \code{outputLocation} creation,
#'     \code{command}.
#'
#' @return 0 if successful
executeCommand <- function(moduleHost, outputLocation, command) {
    if (!inherits(moduleHost, "moduleHost") && !is.null(moduleHost))
        stop("moduleHost object required")
    if (!inherits(outputLocation, "outputLocation") && !is.null(outputLocation))
        stop("outputLocation object required")
    if (!inherits(command, "command"))
        stop("command object required")
    UseMethod("executeCommand")
}

#' @describeIn executeCommand execute a command with no
#'     \code{moduleHost}
#'
#' @export
executeCommand.default <- function(moduleHost, outputLocation, command) {
    system2(command = command$command,
            args = command$args,
            stdout = TRUE, stderr = TRUE)
}
