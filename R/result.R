#' Result of \code{runModule} or \code{runPipeline}
#'
#' @name componentResult
#'
#' @seealso \code{moduleResult}, \code{pipelineResult}
NULL

#' Create \code{moduleResult} object
#'
#' @details This functions is used to construct the objects returned
#'     by \code{runModule}.
#'
#' The \code{export} function can be used to export these objects to a
#' gzipped tarfile. The resulting tarfile can be read loaded into
#' conduit using the \code{importModule} function
#' 
#' @param outputList list of \code{output} objects
#' @param modulePath file path to module output
#' @param module \code{module} object which produced \code{outputList}
#'
#' @return object of class \code{componentResult} and
#'     \code{moduleResult} containing:
#' 
#' \item{name}{module name}
#' \item{file}{file path to resulting module XML}
#' \item{component}{resulting \code{module} object}
#' \item{outputList}{list of \code{output} objects produced by module}
#' \item{execLanguageVersion}{executing language information}
#'
#' @seealso \code{pipelineResult}, \code{module}, \code{runModule},
#'     \code{export}, \code{importModule}
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
    moduleLanguage <- getLanguage(module)
    description <- getDescription(module)
    execLanguageVersion <- getExecLanguageVersion(modulePath)
    
    ## create result module
    moduleInputList <- lapply(outputList, resultInput, modulePath = modulePath)
    moduleInputList <- moduleInputList[!sapply(moduleInputList, is.null)]
    moduleSourceList <-
        lapply(outputList, resultSource, modulePath = modulePath)
    moduleSourceList <- moduleSourceList[!sapply(moduleSourceList, is.null)]
    moduleOutputList <- lapply(outputList, resultOutput)
    resultModule <- module(
        name = name,
        language = moduleLanguage(language = getLanguage(moduleLanguage),
                                  version = execLanguageVersion$execVersion),
        description = description,
        inputs = if (length(moduleInputList)) moduleInputList,
        sources = if (length(moduleSourceList)) moduleSourceList,
        outputs = if (length(moduleOutputList)) moduleOutputList)
    moduleFile <- saveModule(resultModule, targetDirectory = modulePath)

    ## return result module and outputs
    moduleResult <- list(name = name, file = moduleFile,
                         component = resultModule,
                         outputList = outputList,
                         execLanguageVersion = execLanguageVersion)
    class(moduleResult) <- c("moduleResult", "componentResult")
    moduleResult
}

#' Recovers module execution language information
#'
#' Interprets the \file{.languageVersion} file produced when module
#' sources script is executed.
#'
#' The \file{.languageVersion} file should contain four lines of text:
#' 
#' \enumerate{
#'     \item the exact version of the language used for execution
#'     \item \samp{1} if language did not meet minVersion, else \samp{0}
#'     \item \samp{1} if language did not meet maxVersion, else \samp{0}
#'     \item \samp{1} if language did not match version, else \samp{0}
#' }
#'
#' @param modulePath module output file directory
#'
#' @return list containing:
#' \item{\code{execVersion}}{exact version of executing language as character}
#' \item{\code{failMain}}{\code{TRUE} if language did not meet minVersion}
#' \item{\code{failMax}}{\code{TRUE} if language did not meet maxVersion}
#' \item{\code{failExact}}{\code{TRUE} if language did not match version}
#'
#' @seealso prepareScriptInit
getExecLanguageVersion <- function(modulePath) {
    if (!dir.exists(modulePath))
        stop("modulePath does not exist")
    dotLanguageVersion <- file.path(modulePath, ".languageVersion")
    if (!file.exists(dotLanguageVersion))
        stop(".languageVersion file does not exist")
    
    languageVersion <- readLines(dotLanguageVersion)
    execVersion <- languageVersion[1]
    failMin <- as.logical(as.numeric(languageVersion[2]))
    failMax <- as.logical(as.numeric(languageVersion[3]))
    failExact <- as.logical(as.numeric(languageVersion[4]))
    list(execVersion = execVersion,
         failMin = failMin,
         failMax = failMax,
         failExact = failExact)
}

#' Create \code{pipelineResult} object
#'
#' @details This function is used to construct the objects returned
#'     by \code{runPipeline}.
#'
#' The \code{export} function can be used to export these objects to a
#' gzipped tarfile. The resulting tarfile can be read loaded into
#' conduit using the \code{importPipeline} function
#'
#' @param componentResultList list of \code{componentResult} objects
#' @param pipelinePath file path to pipeline output
#' @param pipeline \code{pipeline} object which produced
#'     \code{componentResultList}
#'
#' @return object of class \code{componentResult} and
#'     \code{pipelineResult} containing:
#' 
#' \item{name}{module name}
#' \item{file}{file path to resulting pipeline XML}
#' \item{component}{resulting \code{pipeline} object}
#' \item{outputList}{list of \code{output} objects produced by components}
#' \item{componentResultList}{list of \code{componentResult} objects}
#'
#' @seealso \code{moduleResult}, \code{pipeline}, \code{runPipeline},
#'     \code{export}, \code{importPipeline}
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
               ref <- getRef(output)
               ref <-
                   if (dirname(ref) == modulePath) {
                       basename(ref)
                   } else {
                       gsub(modulePath, ".", ref)
                   }        
               moduleInput(
                   name = name,
                   vessel = fileVessel(ref = ref),
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
    
    ref <- getRef(output)
    ref <-
        if (dirname(ref) == modulePath) {
            basename(ref)
        } else {
            gsub(modulePath, ".", ref)
        }
    input <- moduleInput(name = getName(output), vessel = getVessel(output),
                         format = getFormat(output))
    script <- prepareScriptInput(input,
                                 moduleLanguage = getLanguage(output))
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


