#' Create \code{componentResult} objects
#'
#' Constructor functions to create \code{componentResult} objects.
#'
#' These functions are used to construct the objects returned by
#'     \code{runModule} and \code{runPipeline}.
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
#' The \code{export} function can be used to export these objects to a
#' gzipped tarfile. The resulting tarfile can be read loaded into
#' conduit using the \code{importModule} and \code{importPipeline}
#' functions.
#'
#' @name componentResult
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
    moduleResult <- list(name = name, file = moduleFile,
                         component = resultModule,
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

    name <- getName(pipeline)

    componentList <- lapply(componentResultList, resultComponent,
                            pipelinePath = pipelinePath)

    outputList <- lapply(
        componentResultList,
        function(result) {
            result$outputList
        })

    resultPipeline <- pipeline(name = name,
                               description = getDescription(pipeline),
                               components = componentList)

    xmlfile <- savePipeline(resultPipeline, targetDirectory = pipelinePath)

    pipelineResult <- list(name = name, file = xmlfile,
                           component = resultPipeline,
                           outputList = outputList,
                           componentResultList = componentResultList)
    class(pipelineResult) <- c("pipelineResult", "componentResult")
    pipelineResult
}

#' @describeIn export Export a \code{componentResult} object
#'
#' @export
export.componentResult <- function(x, targetDirectory = getwd()) {
    if (!inherits(x, "componentResult"))
        stop("componentResult object required")
    if (!dir.exists(targetDirectory))
        stop("targetDirectory does not exist")

    ## go to parent of directory to be tarballed
    directory <- dirname(x$file)
    oldwd <- setwd(file.path(directory, ".."))
    on.exit(setwd(oldwd))

    ## generate tarball filename
    name <- getName(x$component)
    tarfile <- file.path(targetDirectory,
                         paste(name, "tar", "gz", sep = "."))

    ## tarball directory containing componentResult
    files <- list.dirs(name)
    sys <- tar(tarfile, files = files, compression = "gzip")
    if (sys != 0) {
        stop("unable to produce tarball")
    } else {
        tarfile
    }
}

#' Import from a module archive
#'
#' Import a module from an exported module tarfile.
#'
#' This function is used to import modules from gzipped tar archives,
#' created by the \code{export.componentFile} function. The tarfile to
#' be imported must be called \file{MODULENAME.tar.gz}; the module to
#' be loaded from the archive will be called
#' \code{MODULENAME/MODULENAME.xml}. The resulting \code{module}
#' object can be named using the \code{name} argument--it will be
#' called MODULENAME by default.
#'
#' @param tarfile File path to module archive
#' @param name Module name
#'
#' @return \code{module} object
#'
#' @seealso \code{export.componentResult}
#'
#' @export
importModule <- function(tarfile, name) {
    exportName <- gsub(".tar.gz", "", basename(tarfile))
    if (missing(name))
        name <- exportName
    untar(tarfile, exdir = tempdir())
    moduleXML <- file.path(tempdir(), exportName,
                           paste0(exportName, ".xml"))
    loadModule(name = name, ref = moduleXML)
}

#' Import from a pipeline archive
#'
#' Import a pipeline from an exported pipeline tarfile.
#'
#' This function is used to import pipelines from gzipped tar
#' archives, created by the \code{export.componentFile} function. The
#' tarfile to be imported must be called \file{PIPELINENAME.tar.gz};
#' the PIPELINE to be loaded from the archive will be found in the
#' file \code{PIPELINENAME/pipeline.xml}. The resulting
#' \code{pipeline} object can be named using the \code{name}
#' argument--it will be called PIPELINENAME by default.
#'
#' @param tarfile File path to pipeline archive
#' @param name Pipeline name
#'
#' @return \code{pipeline} object
#'
#' @seealso \code{export.componentResult}
#'
#' @export
importPipeline <- function(tarfile, name) {
    exportName <- gsub(".tar.gz", "", basename(tarfile))
    if (missing(name))
        name <- exportName
    untar(tarfile, exdir = tempdir())
    pipelineXML <- file.path(tempdir(), exportName, "pipeline.xml")
    loadPipeline(name = name, ref = pipelineXML)
}

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
    script <- prepareScriptInput(input, language = getLanguage(output))
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
#' @param pipelinePath file path to pipeline output
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


