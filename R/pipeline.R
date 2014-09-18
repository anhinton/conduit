### load, save, run and create pipelines

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
    pipelinePath <- paste0(pipelineDir, pathSep)
    descNodes <- getNodeSet(pipeline, "//description|//oa:description",
                            namespaces=namespaces)
    description <-
        if (length(descNodes)) {
            xmlValue(descNodes[[1]])
        } else {
            character(1)
        }
    componentNodes <- getNodeSet(pipeline, "//component|//oa:component",
                              namespaces=namespaces)
    ## FIXME: need to address inline components
    
    pipeNodes <- getNodeSet(pipeline, "//pipe|//oa:pipe",
                            namespaces=namespaces)

    components <-
        lapply(componentNodes,
               function(m, namespaces, pipelineDir) {
                   name <- getXMLAttr(m, "name")
                   ref <- getXMLAttr(m, "ref")
                   path <- getXMLAttr(m, "path")
                   ## if a path is not given assume this means the xml file
                   ## is found in the same directory as the pipeline xml
                   if (is.null(path)) path <- pipelinePath
                   type <- getXMLAttr(m, "type")
                   component <- component(name=name, ref=ref, path=path,
                                          type=type)
                   ## FIXME: can't handle anon/inline components
                   ## path <-
                   ## FIXME: need to check whether module or pipeline
                   component
                   }, namespaces, pipelineDir)
    names(components) <-
        sapply(components, componentName)
    pipes <-
        lapply(pipeNodes,
               function (x, namespaces) {
                   start <- xmlChildren(x)$start
                   startComponent <- xmlAttrs(start)[["component"]]
                   startOutput <- xmlAttrs(start)[["output"]]
                   end <- xmlChildren(x)$end
                   endComponent <- xmlAttrs(end)[["component"]]
                   endInput <- xmlAttrs(end)[["input"]]
                   pipe(startComponent=startComponent,
                        startOutput=startOutput,
                        endComponent=endComponent,
                        endInput=endInput)
               },
               namespaces)    
    pipeline(name=pipelineName, path=pipelinePath, description=description,
             components=components, pipes=pipes)
}

## functions to write a pipeline (and its modules) to XML files

#' Convert a pipeline to XML
#'
#' @param pipeline \code{pipeline} object
#' @param namespaceDefinitions XML namespaces as character vector
#' @param export if true inline components are converted to reference files
#' @return \code{XMLNode} object
pipelineToXML <- function(pipeline, namespaceDefinitions=NULL, export=FALSE) {
    pipelineRoot <- newXMLNode("pipeline",
                               namespaceDefinitions=namespaceDefinitions)
    description <- newXMLNode("description", pipeline$description)
    ## componentNames <- names(pipeline$components)
    ## when export=T we alter all the components to be name/ref only, assuming
    ## the file will be called COMP_NAME.xml with no path info
    components <-
        lapply(pipeline$components,
               function(c) {
                   componentRoot <-
                       newXMLNode("component",
                                  attrs=c(name=c$name))
                   if (export) {
                       xmlAttrs(componentRoot) <-
                           c(ref=paste0(c$name, ".xml"),
                             type=c$type)
                   } else if (!is.null(c$ref)) {
                       xmlAttrs(componentRoot) <-
                           c(ref=paste0(c$name, ".xml"),
                             path=c$path,
                             type=c$type)
                   } else {
                       componentXML <- componentToXML(c)
                       componentRoot <-
                           addChildren(componentRoot,
                                       kids=list(componentXML))
                   }
                   componentRoot
               })
    pipes <-
        lapply(pipeline$pipes,
               function (p) {
                   startAttrs <- c("component"=p$start$component,
                                   "output"=p$start$output)
                   endAttrs <- c("component"=p$end$component,
                                 "input"=p$end$input)
                   pipe <- newXMLNode("pipe")
                   pipe <-
                       addChildren(pipe,
                                   kids=list(
                                       newXMLNode("start", attrs=startAttrs),
                                       newXMLNode("end", attrs=endAttrs)))
               })
    pipelineRoot <- addChildren(pipelineRoot,
                                kids=list(description, components, pipes))
    pipelineRoot
}

#' Save a pipeline to disk
#'
#' Saves a \code{pipeline} to disk as an openapi XML file
#'
#' If \code{export} is true the resulting pipeline file will have all
#' components in name/ref format, assuming the component XML files
#' have been saved to the \file{targetDirectory}.
#'
#' As at 2014-08-12 the resulting file is always called \file{pipeline.xml}
#'
#' @param pipeline \code{pipeline} object
#' @param targetDirectory file location to save output
#' @param export boolean determining whether to keep components inline
#' @import XML
#' @export
savePipeline <- function(pipeline, targetDirectory=getwd(), export=FALSE) {
    if (!file.exists(targetDirectory)) {
        stop("no such target directory")
    }
    pipelineDoc <-
        newXMLDoc(namespaces="http://www.openapi.org/2014",
                  node=pipelineToXML(pipeline=pipeline, export=export,
                      namespaceDefinitions="http://www.openapi.org/2014/"))
    pipelineFilePath <-
        file.path(targetDirectory,
                  "pipeline.xml")
    saveXML(pipelineDoc, pipelineFilePath)
}

#' Wrie a pipeline and its modules to disk
#'
#' Write a \code{pipeline} and its \code{module}s to disk as openapi XML files
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
#' @export
exportPipeline <- function(pipeline, targetDirectory) {
    pipelineDirectory <- file.path(targetDirectory, componentName(pipeline))
    if (!file.exists(pipelineDirectory)) {
        dir.create(pipelineDirectory)
    ## ## FIXME: I was producing this warning every time AKA ignoring it, so it
    ## ## would be far better if I got some real warnings and errors and what
    ## ## have you in place
    ## } else {
    ##     warning("this pipeline directory exists and you might be writing over something useful")
    }
    pipelineFile <- savePipeline(pipeline, pipelineDirectory, export=TRUE)
    result <- c(pipeline=pipelineFile,
                lapply(pipeline$components, exportComponent, pipelineDirectory))
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
                   paste(x$end$component, x$end$input, sep=".")
               })
    inputsList <-
        lapply(pipes,
               function (x, components, pipelinePath) {
                   endComponent <- components[[x$end$component]]
                   platform <- endComponent$value$platform[["name"]]
                   type <- endComponent$value$inputs[[x$end$input]][["type"]]
                   ## FIXME: this assumes the start component is a module
                   ## and can be found in a "modules" folder. Needs to account
                   ## for pipelines
                   if (type == "internal") {
                       input <- file.path(pipelinePath, "modules",
                                          x$start$component,
                                          paste(x$start$output,
                                                internalExtension(platform),
                                                sep=""))
                   } else if (type == "external") {
                       startComponent <- components[[x$start$component]]
                       ref <-
                           startComponent$value$outputs[[x$start$output]]["ref"]
                       path <- startComponent$value$path
                       input <- if (dirname(ref) == ".") {
                           file.path(pipelinePath, "modules",
                                     x$start$component, ref)
                       } else {
                           findFile(ref, amendSearchPaths(path))
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
                   pipe <- c(x$start$component, x$start$output,
                             x$end$component, x$start$output)
                   names(pipe) <- c("startComponent", "startOutput",
                                    "endComponent", "endInput")
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
    pipelineName <- componentName(pipeline)
    pipelinePath <- file.path("pipelines", pipelineName)
    if (file.exists(pipelinePath))
        unlink(pipelinePath, recursive=TRUE)
    dir.create(pipelinePath, recursive=TRUE)
    pipelinePath <- tools::file_path_as_absolute(pipelinePath)
    components <- pipeline$components
    ## load component values
    components <-
        lapply(
            components,
            function (c) {
                if (!is.null(c$ref)) {
                    if (is.null(c$path)) c$path <- searchPaths
                    ## FIXME: only handles modules, not pipelines
                    c <- loadComponent(c)
                }
                c
            })
    componentNames <- names(components)
    ## making a graph of the pipeline to determine order
    componentGraph <- graphPipeline(pipeline)
    componentOrder <- RBGL::tsort(componentGraph)
    inputs <- inputsList(pipeline$pipes, components, pipelinePath)
    x <-
        lapply(componentOrder,
               function (x, components, inputs, pipelinePath) {
                   ## select inputs for this component and strip out
                   ## component name
                   component <- components[[x]]
                   ## FIXME: selecting inputs from inputsList seems a little
                   ## inelegant. Possibly calculating all input locations
                   ## before anything is run is the reason for this.
                   ## What else shall we try?
                   whichInputs <-
                       grepl(paste0("^", componentName(component),"[.]"),
                             names(inputs))
                   inputs <- inputs[whichInputs]
                   names(inputs) <-
                       gsub(paste0("^", componentName(component),"[.]"), "",
                            names(inputs))
                   ## run the beast
                   runComponent(component, inputs, pipelinePath)
               }, components, inputs, pipelinePath)
}

## creating new pipelines

#' Create a pipe object
#'
#' @param startComponent Name of start module
#' @param startOutput Name of start object
#' @param endComponent Name of end module
#' @param endInput Name of end input
#' @return \code{pipe} connecting \code{startComponentName}.\code{startOutputName} to \code{endComponentName}.\code{endInputName}
#' @export
pipe <- function (startComponent, startOutput,
                  endComponent, endInput) {
    start <- list(component=startComponent, output=startOutput)
    end <- list(component=endComponent, input=endInput)
    pipe <- list(start=start, end=end)
    class(pipe) <- "pipe"
    pipe
}


#' Add a new component to a pipeline
#'
#' @param newComponent \code{pipeline} or \code{module} object to be added
#' @param pipeline \code{pipeline} to be amended
#' @return \code{pipeline} object
#' @export
addComponent <- function(newComponent, pipeline) {
    name <- componentName(newComponent)
    if (class(newComponent) != "component") {
        newComponent <- component(name, value=newComponent)
    }
    componentNames <- c(names(pipeline$components), name)    
    pipeline$components <- c(pipeline$components, temp=list(newComponent))
    names(pipeline$components) <- componentNames
    pipeline
}

#' add a pipe to a pipeline
#'
#' Add a \code{pipe} object to a \code{pipeline}
#'
#' @param newPipe \code{pipe} object
#' @param pipeline \code{pipeline} object
#' @return \code{pipeline} object
#' @export
addPipe <- function(newPipe, pipeline) {
    pipeline$pipes <- c(pipeline$pipes, list(newPipe))
    pipeline
}

#' return the name of a component
#'
#' Returns the name of a \code{module} or \pipeline{pipeline}
#'
#' @param component \code{module} or \code{pipeline} object
#' @return character value
componentName <- function (component) {
    component$name
}

#' Create a pipeline
#'
#' Create an openapi \code{pipeline} object
#'
#' If \code{components} is not specified, \code{pipeline} will construct
#' it the compoments from \code{modules} and \code{pipelines}.
#'
#' @param name \code{pipeline} name
#' @param path location of originating pipeline xml file
#' @param description \code{pipeline} description
#' @param components list of \code{module}s and \code{pipeline}s
#' @param modules list of \code{module}s
#' @param pipelines list of \code{pipeline}s
#' @param pipes list of \code{pipe}s
#' @return \code{pipeline} list containing:
#' \item{name}{character value}
#' \item{path}{Location of source pipeline XML file}
#' \item{description}{character value}
#' \item{components}{list of \code{module}s and \code{pipeline}s}
#' \item{pipes}{list of \code{pipe}s}
#' @export
pipeline <- function (name, path=NULL, description="", components=list(),
                      modules=list(), pipelines=list(), pipes=list()) {
    if (!length(components)) {
        components <- c(modules, pipelines)
    } 
    names(components) <- sapply(components, componentName)
    components <-
        lapply(components, function(c) {
            if (class(c) == "component") {
                component <- c
            } else {
                component <- component(c$name, c)
            }
            component
        })
    pipeline <- list(name=name, path=path, description=description,
                     components=components, pipes=pipes)
    class(pipeline) <- "pipeline"
    pipeline
}
