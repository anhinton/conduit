#' Return a \code{moduleInput} object for use in a
#' \code{moduleResult} object.
#'
#' This function constructs an appropriate \code{moduleInput} object
#' from the \code{output} object created when executing a module
#' source.
#'
#' @param output \code{output} object
#' @param modulePath file path to module output
#'
#' @return \code{moduleInput} object or NULL
resultInput <- function(output, modulePath) {
    if (!inherits(output, "output"))
        stop("output object required")
    if (!dir.exists(modulePath))
        stop("modulePath does not exist")
    
    name <- getName(output)
    vessel <- getVessel(output)
    type <- getType(vessel)
    format <- getFormat(output)
    switch(type,
           fileVessel =, internalVessel = {
               result <- getResult(output)
               resultref <-
                   if (dirname(result) == modulePath) {
                       basename(result)
                   } else {
                       gsub(modulePath, ".", result)
                   }        
               moduleInput(
                   name = name,
                   vessel = fileVessel(ref = resultref),
                   format = format)
           })
}

#' Return a \code{moduleSource} object for use in a
#' \code{moduleResult} object.
#'
#' This function constructs an appropriate \code{moduleSource} object
#' from the \code{output} object created when executing a module
#' source.
#'
#' @param output \code{output} object
#' @param modulePath file path to module output
#'
#' @return \code{moduleSouce} object or NULL
resultSource <- function(output, modulePath) {
    if (!inherits(output, "output"))
        stop("output object required")
    if (!dir.exists(modulePath))
        stop("modulePath does not exist")
    
    result <- getResult(output)
    resultref <-
        if (dirname(result) == modulePath) {
            basename(result)
        } else {
            gsub(modulePath, ".", result)
        }
    input <- moduleInput(name = getName(output), vessel = getVessel(output),
                         format = getFormat(output))
    script <- prepareScriptInput(input, inputObject = resultref,
                                 language = getLanguage(output))
    if (!is.null(script)) {
        return(moduleSource(scriptVessel(script)))
    } else {
        NULL
    }

}

#' Return a \code{moduleOutput} object for use in a
#' \code{moduleResult} object.
#'
#' @param output \code{output} object
#'
#' @return \code{moduleOutput} object
resultOutput <- function(output) {
    if (!inherits(output, "output"))
        stop("output object required")
    
    moduleOutput(name = getName(output), vessel = getVessel(output),
                 format = getFormat(output))
}

moduleResult <- function(objects, modulePath, module) {
    name <- getName(module)
    language <- getLanguage(module)
    description <- getDescription(module)
    
    ## create result module
    inputList <- lapply(objects, resultInput, modulePath = modulePath)
    inputList <- inputList[!sapply(inputList, is.null)]
    sourceList <- lapply(objects, resultSource, modulePath = modulePath)
    sourceList <- sourceList[!sapply(sourceList, is.null)]
    outputList <- lapply(objects, resultOutput)
    resultModule <- module(
        name = name,
        language = language,
        description = description,
        inputs = inputList,
        sources = sourceList,
        outputs = outputList)
    moduleFile <- saveModule(resultModule, targetDirectory = modulePath)

    ## return result module and outputs
    moduleResult <- list(file = moduleFile, module = resultModule,
                         objects = objects)
    class(moduleResult) <- "moduleResult"
    moduleResult
}

pipelineResult <- function() {
    
}

