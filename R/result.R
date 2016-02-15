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

#' Return a \code{component} object for use in a \code{pipelineResult}
#' object.
#'
#' @param componentResult \code{componentResult} object
#'
#' @return \code{component} object
resultComponent <- function(componentResult, pipelinePath) {
    if (!inherits(componentResult, "componentResult"))
        stop("componentResult object require")
    if (!dir.exists(pipelinePath))
        stop("pipelinePath does not exist")
    
    value <- componentResult$component
    xmlfile <- gsub(pipelinePath, ".", componentResult$file)
    vessel <- fileVessel(xmlfile)
    component(value = value, vessel = vessel)
}

#' @describeIn componentResult returns the result of running a
#'     \code{module}
#'
#' @export
moduleResult <- function(outputList, modulePath, module) {
    if (any(!sapply(outputList, inherits, what = "output")))
        stop("outputList must be 'output' objects")
    if (!dir.exists(modulePath))
        stop("modulePath does not exist")
    if (!inherits(module, "module"))
        stop("module object required")
    
    name <- getName(module)
    language <- getLanguage(module)
    description <- getDescription(module)
    
    ## create result module
    moduleInputList <- lapply(outputList, resultInput, modulePath = modulePath)
    moduleInputList <- moduleInputList[!sapply(moduleInputList, is.null)]
    moduleSourceList <-
        lapply(outputList, resultSource, modulePath = modulePath)
    moduleSourceList <- moduleSourceList[!sapply(moduleSourceList, is.null)]
    moduleOutputList <- lapply(outputList, resultOutput)
    resultModule <- module(
        name = name,
        language = language,
        description = description,
        inputs = if (length(moduleInputList)) moduleInputList,
        sources = if (length(moduleSourceList)) moduleSourceList,
        outputs = if (length(moduleOutputList)) moduleOutputList)
    moduleFile <- saveModule(resultModule, targetDirectory = modulePath)

    ## return result module and outputs
    moduleResult <- list(file = moduleFile, component = resultModule,
                         outputList = outputList)
    class(moduleResult) <- c("moduleResult", "componentResult")
    moduleResult
}

#' @describeIn componentResult returns the result of running a
#'     \code{pipeline}
#'
#' @export
pipelineResult <- function(componentResultList, pipelinePath, pipeline) {
    if (!all(sapply(
             componentResultList,
             inherits,
             what = "componentResult"))) {
        stop("componentResultList must contain componentResult objects")
    }
    if (!dir.exists(pipelinePath))
        stop("pipelinePath does not exist")
    if (!inherits(pipeline, "pipeline"))
        stop("pipeline must be a pipeline object")

    componentList <- lapply(componentResultList, resultComponent,
                            pipelinePath = pipelinePath)

    outputList <- lapply(
        componentResultList,
        function(result) {
            result$outputList
        })

    resultPipeline <- pipeline(name = getName(pipeline),
                               description = getDescription(pipeline),
                               components = componentList)

    xmlfile <- savePipeline(resultPipeline, targetDirectory = pipelinePath)

    pipelineResult <- list(file = xmlfile, component = resultPipeline,
                           outputList = outputList)
    class(pipelineResult) <- c("pipelineResult", "moduleResult")
    pipelineResult
}

#' @name componentResult
#'
#' @title Create \code{componentResult} objects
#'
#' @description Constructor functions to create \code{componentResult}
#'     objects.
#'
#' @details These functions are used to construct the objects returned
#'     by \code{runModule} and \code{runPipeline}.
#'
#' The \code{moduleResult} constructor takes a list of module
#' \code{output} objects created by \code{resolveOutput}, the original
#' \code{module}, and the module's output location
#' \code{modulePath}. A \code{moduleResult} object is returned.
#'
#' The \code{pipelineResult} constructor takes a list of
#' \code{componentResult} objects produced by \code{runComponent}, the
#' original \code{pipeline}, and the pipeline's output location
#' \code{pipelinePath}. A \code{pipelineResult} object is returned.
#'
#' @param outputList list of \code{output} objects
#' @param modulePath file path to module output
#' @param module \code{module} object which produced \code{outputList}
#' @param componentResultList list of \code{componentResult} objects
#' @param pipelinePath file path to pipeline output
#' @param pipeline \code{pipeline} object which produced
#'     \code{componentResultList}
#'
#' @return object of class \code{componentResult} and either
#'     \code{moduleResult} or \code{componentResult} containing:
#' 
#' \item{file}{file path to resulting module or pipeline XML}
#' \item{component}{resulting \code{module} or \code{pipeline} object}
#' \item{outputList}{list of \code{output} objects produced by
#'     module(s)}
NULL
