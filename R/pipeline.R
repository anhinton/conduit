### load, save, run and create pipelines

## loadPipeline()
## arguments:
## - filename
## - pipelineName
## - namespaces
## description:
##   load a pipeline and its modules from an .xml file
##   returns a list of:
##   - name
##   - description
##   - modules
##   - pipes
#' Load a pipeline and its modules from disk
#'
#' Read an openapi \code{pipeline} and its associated \code{module}s from an
#' XML document
#'
#' @param filename file path to \code{pipeline} XML
#' @param pipelineName as character value
#' @param namespaces named charactor vector
#' @return \code{pipeline} list containing:
#' \item{name}{as character value}
#' \item{description}{as character value}
#' \item{components}{list of \code{module}s and \code{pipeline}s}
#' \item{pipes}{list of \code{pipe}s}
#' @import XML
#' @export
loadPipeline <-
    function(filename,
             pipelineName=tools::file_path_sans_ext(basename(filename)),
             namespaces=c(oa="http://www.openapi.org/2014/")) {
    filename <- tools::file_path_as_absolute(filename)
    pipelineDir <- dirname(filename)
    pipeline <- xmlRoot(xmlParse(filename))
    descNodes <- getNodeSet(pipeline, "//description|//oa:description",
                            namespaces=namespaces)
    description <-
        if (length(descNodes)) {
            xmlValue(descNodes[[1]])
        } else {
            character(1)
        }
    moduleNodes <- getNodeSet(pipeline, "//module|//oa:module",
                              namespaces=namespaces)
    
    pipeNodes <- getNodeSet(pipeline, "//pipe|//oa:pipe",
                            namespaces=namespaces)
    modules <-
        lapply(moduleNodes,
               function(m, namespaces) {
                   attrs <- xmlAttrs(m)
                   name <-
                       if (any(names(attrs) == "name")) {
                           attrs[["name"]]
                       } else {
                           basename(attrs[["ref"]])
                       }
                   ref <-
                       if (any(names(attrs) == "ref")) {
                           attrs[["ref"]]
                       } else {
                           paste0(attrs[["name"]], ".xml")
                       }
                   path <-
                       if (any(names(attrs) == "path")) {
                           amendSearchPaths(
                               defaultSearchPaths,
                               attrs[["path"]])
                       } else {
                           ## FIXME: this should make use of amendSearchPaths()
                           amendSearchPaths(
                               defaultSearchPaths,
                               paste0(pipelineDir, pathSep))
                       }
                   loadModule(name, ref, path, namespaces)
                   }, namespaces)
    names(modules) <-
        sapply(modules,
               function(m) {
                   m$name
               })
    pipes <-
        lapply(pipeNodes,
               function (x, namespaces) {
                   ## FIXME: assumes all pipes contain only
                   ## start/endComponentName, and not
                   ## start/endComponetRef. Need to write this case.
                   start <- xmlChildren(x)$start
                   startComponentName <- xmlAttrs(start)[["component-name"]]
                   startOutputName <- xmlAttrs(start)[["output-name"]]
                   end <- xmlChildren(x)$end
                   endComponentName <- xmlAttrs(end)[["component-name"]]
                   endInputName <- xmlAttrs(end)[["input-name"]]
                   pipe(startComponentName=startComponentName,
                        startOutputName=startOutputName,
                        endComponentName=endComponentName,
                        endInputName=endInputName)
               },
               namespaces)    
    pipeline(name=pipelineName, description=description,
             components=modules, pipes=pipes)
}

## functions to write a pipeline (and its modules) to XML files

#' Save a pipeline to disk
#'
#' Saves a \code{pipeline} to disk as an openapi XML file
#'
#' As at 2014-08-12 the resulting file is always called \file{pipeline.xml}
#' 
#' @import XML
#' @export
savePipeline <- function(pipeline, targetDirectory=getwd()) {
    pipelineDoc <-
        newXMLDoc(namespaces="http://www.openapi.org/2014",
                  node=newXMLNode("pipeline", attrs=c(name=pipeline$name),
                      namespaceDefinitions="http://www.openapi.org/2014/"))
    pipelineRoot <- xmlRoot(pipelineDoc)
    description <- newXMLNode("description", pipeline$description)
    componentNames <- names(pipeline$components)
    components <-
        addChildren(
            newXMLNode("components"),
            kids=lapply(pipeline$components,
                function (c) {
                    if (class(c) == "module") {
                        newXMLNode("module", attrs=c(name=c$name))
                    } else if (class(c) == "pipeline") {
                        newXMLNode("pipeline", attrs=c(name=c$name))
                    }
                }))
    pipes <-
        lapply(pipeline$pipes,
               function (p) {
                   startAttrs <- c("component-name"=p$start$componentName,
                                   "output-name"=p$start$outputName)
                   endAttrs <- c("component-name"=p$end$componentName,
                                 "input-name"=p$end$inputName)
                   pipe <- newXMLNode("pipe")
                   pipe <-
                       addChildren(pipe,
                                   kids=list(
                                       newXMLNode("start", attrs=startAttrs),
                                       newXMLNode("end", attrs=endAttrs)))
               })
    pipelineRoot <- addChildren(pipelineRoot,
                                kids=list(description, components, pipes))
    pipelineFilePath <-
        file.path(targetDirectory,
                  "pipeline.xml")
    saveXML(pipelineDoc, pipelineFilePath)
}

#' Save a pipeline and its modules to disk
#'
#' Save a \code{pipeline} and its \code{module}s to disk as openapi XML files
#'
#' Creates a directory named for the \code{pipeline} in \code{targetDirectory},
#' then saves \code{pipeline} and \code{module} XML files in this directory.
#'
#' As at 2014-08-12 the \code{pipeline} is always saved as \file{pipeline.xml}
#' no matter what the \code{pipeline} name.
#'
#' @param pipeline A \code{pipeline} list
#' @param targetDirectory Output directory path
#' @return A list of the XML file paths written
exportPipeline <- function(pipeline, targetDirectory) {
    pipelineDirectory <- file.path(targetDirectory, pipeline$name)
    if (!file.exists(pipelineDirectory)) {
        dir.create(pipelineDirectory)
    ## ## FIXME: I was producing this warning every time AKA ignoring it, so it
    ## ## would be far better if I got some real warnings and errors and what
    ## ## have you in place
    ## } else {
    ##     warning("this pipeline directory exists and you might be writing over something useful")
    }
    pipelineFile <- savePipeline(pipeline, pipelineDirectory)
    result <- c(pipeline=pipelineFile,
                lapply(pipeline$components, saveModule, pipelineDirectory))
    result
}

## functions to run a loaded PIPELINE

## internalExtension()
## arguments:
## - platform: character
## description
##   select the right file extension for internal objects of a given platform.
##   returns a character value of correct extension
internalExtension <- function(platform) {
    extension <- switch(platform,
                        R = ".rds",
                        shell = ".txt")
    extension
}

#' Returns a named list of input addresses
#'
#' \code{inputsList} returns a named list of absolute file locations for
#' components' inputs.
#' 
#' List items are named as COMPONENT_NAME.INPUT_NAME
#'
#' @param pipes List of \code{pipe}s
#' @param components List of \code{component}s
#' @param pipelinePath Absolute file path to currently running \code{pipeline}
inputsList <- function(pipes, components, pipelinePath) {
    inputNames <-
        lapply(pipes,
               function (x) {
                   paste(x$end$componentName, x$end$inputName, sep=".")
               })
    inputsList <-
        lapply(pipes,
               function (x, components, pipelinePath) {
                   endComponent <- components[[x$end$componentName]]
                   platform <- endComponent$platform["name"]
                   type <- endComponent$inputs[[x$end$inputName]]["type"]
                   ## FIXME: this assumes the start component is a module
                   ## and can be found in a "modules" folder. Needs to account
                   ## for pipelines
                   if (type == "internal") {
                       input <- file.path(pipelinePath, "modules",
                                          x$start$componentName,
                                          paste(x$start$outputName,
                                                internalExtension(platform),
                                                sep=""))
                   } else if (type == "external") {
                       startComponent <- components[[x$start$componentName]]
                       input <-
                           startComponent$outputs[[x$start$outputName]]["ref"]
                       if (dirname(input) == ".") {
                           input <- file.path(pipelinePath, "modules",
                                              x$start$componentName, input)
                       }
                   }
                   input
               }, components, pipelinePath)
    names(inputsList) <- inputNames
    inputsList
}

#' Create a graph of a pipeline
#'
#' \code{graphPipeline} produces a directed graph of the given
#' \code{pipeline} with components as nodes and pipes as directed edges
#'
#' @param pipeline A \code{pipeline} list object
#'
#' @return A \pkg{graph} \code{graphNEL} object
#' @import graph
graphPipeline <- function(pipeline) {
    componentNames <- names(pipeline$components)
    pipes.list <-
        lapply(pipeline$pipes,
               function (x) {
                   pipe <- c(x$start$componentName, x$start$outputName,
                             x$end$componentName, x$start$outputName)
                   names(pipe) <- c("startComponentName", "startOutput",
                                    "endComponentName", "endInput")
                   pipe
               })
    pipes.matrix <- do.call(rbind, pipes.list)
    edgeList <-
        lapply(componentNames,
               function (startComponentName, pipes) {
                   isStartComponent <- pipes[,1] == startComponentName
                   list(edges=pipes[isStartComponent,3])
               },
               pipes.matrix)
    names(edgeList) <- componentNames
    new("graphNEL", nodes=componentNames, edgeL=edgeList,
        edgemode="directed")
}

#' Run a pipeline
#' 
#' @details
#' This function will attempt to run an OpenAPI \code{pipeline} list object,
#' either loaded from a pipeline .XML file with \code{loadPipeline} or created
#' with \code{pipeline} function.
#'
#' The function should produce a directory called \file{pipelines} in the
#' working directory where it will produce its \code{module}s' results.
#'
#' First the function will determine the order in which its modules are to
#' be run. Note that the \code{pipeline} is not allowed to have any cycles or
#' the function will fail.
#'
#' The function then determines the file paths of the objects described in
#' the \code{pipe}s as inputs/outputs.
#'
#' Finally the function runs each \code{module} in the order determined,
#' feeding each \code{module} its inputs. A directory will be created for each
#' \code{module} in \file{pipelines/modules}, in which the function will save
#' the script used to run the \code{module}, and its outputs.
#'
#' @param pipeline A \code{pipeline} list object
#' @return Meaningless list. TODO: fix what \code{runPipeline},
#' \code{runModule}, \code{runPlatform} return.
#' @export
runPipeline <- function(pipeline) {
    if (!file.exists("pipelines")) dir.create("pipelines")
    pipelineName <- pipeline$name
    pipelinePath <- file.path("pipelines", pipelineName)
    if (file.exists(pipelinePath))
        unlink(pipelinePath, recursive=TRUE)
    dir.create(pipelinePath, recursive=TRUE)
    pipelinePath <- tools::file_path_as_absolute(pipelinePath)
    ## FIXME: components can be modules or pipelines, but as of 2014-08-12
    ## we will assume only modules
    modules <- pipeline$components
    moduleNames <- names(modules)
    ## making a graph of the pipeline to determine order
    moduleGraph <- graphPipeline(pipeline)
    moduleOrder <- RBGL::tsort(moduleGraph)
    inputs <- inputsList(pipeline$pipes, modules, pipelinePath)
    x <-
        lapply(moduleOrder,
               function (x, modules, inputs, pipelinePath) {
                   ## select inputs for this module and strip out module name
                   module <- modules[[x]]
                   ## FIXME: selecting inputs from inputsList seems a little
                   ## inelegant. Possibly calculating all input locations
                   ## before anything is run is the reason for this.
                   ## What else shall we try?
                   whichInputs <-
                       grepl(paste0("^", module$name,"[.]"), names(inputs))
                   inputs <- inputs[whichInputs]
                   names(inputs) <-
                       gsub(paste0("^",module$name,"[.]"), "", names(inputs))
                   ## run the beast
                   runModule(module, inputs, pipelinePath)
               }, modules, inputs, pipelinePath)
}

## creating new pipelines

#' Create a pipe object
#'
#' @param startComponentName Name of start module
#' @param startOutputName Name of start object
#' @param endComponentName Name of end module
#' @param endInputName Name of end input
#' @param startComponentRef Address of start module
#' @param endComponentRed Address of end module
#' @return \code{pipe} connecting \code{startComponentName}.\code{startOutputName} to \code{endComponentName}.\code{endInputName}
#' @export
pipe <- function (startComponentName=NULL, startOutputName,
                  endComponentName=NULL, endInputName,
                  startComponentRef=NULL, endComponentRef=NULL) {
    start <-
        if (is.null(startComponentName)) {
            list(componentRef=startComponentRef, outputName=startOutputName)
        } else {
            list(componentName=startComponentName, outputName=startOutputName)
        }
    end <-
        if (is.null(endComponentName)) {
            list(componentRef=endComponentRef, inputName=endInputName)
        } else {
            list(componentName=endComponentName, inputName=endInputName)
        }
    pipe <- list(start=start, end=end)
    class(pipe) <- "pipe"
    pipe
}


## adds a module object list to a pipeline
#' @export
addModule <- function(newModule, pipeline) {
    moduleNames <- c(names(pipeline$components), newModule$name)
    pipeline$modules <- c(pipeline$components, temp=list(newModule))
    names(pipeline$components) <- moduleNames
    pipeline
}

## adds a pipe object list to a pipeline
#' @export
addPipe <- function(newPipe, pipeline) {
    pipeline$pipes <- c(pipeline$pipes, list(newPipe))
    pipeline
}

## pipeline()
## arguments:
## - name: character
## - desctription: character
## - modules: list of modules made with module() or loadModule()
## - pipes: list of pipes made with pipe()
## description:
##   Constructs a pipeline list.
##   Names modules with module$name.
#' @export
pipeline <- function (name, description="", components=list(), pipes=list()) {
    names(components) <- sapply(components, function(c) { c$name })
    pipeline <- list(name=name, description=description, components=components,
                     pipes=pipes)
    class(pipeline) <- "pipeline"
    pipeline
}
