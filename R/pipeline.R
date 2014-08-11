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
loadPipeline <- function(filename,
                         pipelineName=file_path_sans_ext(basename(filename)),
                         namespaces=c(oa="http://www.openapi.org/2014/")) {
    filename <- file_path_as_absolute(filename)
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
                           attrs[["path"]]
                       } else {
                           ## FIXME: this should make use of amendSearchPaths()
                           paste0(pipelineDir, pathSep, defaultSearchPaths)
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
                   start <- xmlChildren(x)$start
                   startModule <- xmlAttrs(start)[["module"]]
                   startName <- xmlAttrs(start)[["name"]]
                   start <- c("module"=startModule, "name"=startName)
                   end <- xmlChildren(x)$end
                   endModule <- xmlAttrs(end)[["module"]]
                   endName <- xmlAttrs(end)[["name"]]
                   end <- c("module"=endModule, "name"=endName)
                   list("start"=start, "end"=end)
               },
               namespaces)    
    pipeline(name=pipelineName, description=description,
             components=modules, pipes=pipes)
}

## functions to write a pipeline (and its modules) to XML files

## savePipeline()
## arguments:
## - pipeline
## - targetDirectory: where to save .xml file
## description:
##   saves a pipeline to a pipeline .xml file
##   NB: always called 'pipeline.xml'
##   TODO: build in overwrite warning; provide option for different name?
savePipeline <- function(pipeline, targetDirectory=getwd()) {
    pipelineDoc <-
        newXMLDoc(namespaces="http://www.openapi.org/2014",
                  node=newXMLNode("pipeline", attrs=c(name=pipeline$name),
                      namespaceDefinitions="http://www.openapi.org/2014/"))
    pipelineRoot <- xmlRoot(pipelineDoc)
    description <- newXMLNode("description", pipeline$description)
    moduleNames <- names(pipeline$modules)
    modules <- lapply(names(pipeline$modules),
                      function (m) {
                          newXMLNode("module", attrs=c(name=m))
                      })
    names(modules) <- NULL
    pipes <-
        lapply(pipeline$pipes,
               function (p) {
                   startAttrs <- c(p$start["module"], p$start["name"])
                   endAttrs <- c(p$end["module"], p$end["name"])
                   pipe <- newXMLNode("pipe")
                   pipe <-
                       addChildren(pipe,
                                   kids=list(
                                       newXMLNode("start", attrs=startAttrs),
                                       newXMLNode("end", attrs=endAttrs)))
               })
    pipelineRoot <- addChildren(pipelineRoot,
                                kids=list(description, modules, pipes))
    pipelineFilePath <-
        file.path(targetDirectory,
                  "pipeline.xml")
    saveXML(pipelineDoc, pipelineFilePath)
}

## exportPipeline()
## arguments:
## - pipeline
## - targetDirectory: where to create named pipeline directory
## description:
##   creates a folder named for the pipeline$name in targetDirectory, then saves
##   pipeline and module .xml files in this directory
exportPipeline <- function(pipeline, targetDirectory) {
    pipelineDirectory <- file.path(targetDirectory, pipeline$name)
    if (!file.exists(pipelineDirectory)) {
        dir.create(pipelineDirectory)
    } else {
        warning("this pipeline directory exists and you might be writing over something useful")
    }
    savePipeline(pipeline, pipelineDirectory)
    lapply(pipeline$modules, saveModule, pipelineDirectory)
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

## inputsList()
## arguments:
## - pipes
## - modules
## - pipelinePath: directory where pipeline will be run
## description:
##   make a list of where module inputs will come from
##   return list of inputs, each item of which is a character value containing
##   a path
inputsList <- function(pipes, modules, pipelinePath) {
    inputNames <-
        lapply(pipes,
               function (x) {
                   paste(x$end["component-name"], x$end["input-name"], sep=".")
               })
    inputsList <-
        lapply(pipes,
               function (x, modules, pipelinePath) {
                   endModule <- modules[[x$end["component-name"]]]
                   platform <- endModule$platform["name"]
                   type <- endModule$inputs[[x$end["component-name"]]]["type"]
                   if (type == "internal") {
                       input <- file.path(pipelinePath, "modules",
                                          x$start["component-name"],
                                          paste(x$start["output-name"],
                                                internalExtension(platform),
                                                sep=""))
                   } else if (type == "external") {
                       startModule <- modules[[x$start["component-name"]]]
                       input <-
                           startModule$outputs[[x$start["output-name"]]]["ref"]
                       if (dirname(input) == ".") {
                           input <- file.path(pipelinePath, "modules",
                                              x$start["component-name"], input)
                       }
                   }
                   input
               }, modules, pipelinePath)
    names(inputsList) <- inputNames
    inputsList
}

## creates a graphNEL object from a pipeline. this is used by runPipeline() to
## determine run order of modules, but should also be used at some future point
## to draw cool pictures of my pipelines
graphPipeline <- function(pipeline) {
    moduleNames <- names(pipeline$components)
    pipes.list <-
        lapply(pipeline$pipes,
               function (x) {
                   startModule <- x$start["component-name"]
                   startOutput <- x$start["output-name"]
                   endModule <- x$end["component-name"]
                   endInput <- x$end["input-name"]
                   pipe <- c(startModule, startOutput, endModule, endInput)
                   names(pipe) <- c("startModule", "startOutput", "endModule",
                                    "endInput")
                   pipe
               })
    pipes.matrix <- do.call(rbind, pipes.list)
    edgeList <-
        lapply(moduleNames,
               function (startModule, pipes) {
                   isStartModule <- pipes[,1] == startModule
                   list(edges=pipes[isStartModule,3])
               },
               pipes.matrix)
    names(edgeList) <- moduleNames
    new("graphNEL", nodes=moduleNames, edgeL=edgeList,
        edgemode="directed")
}

## runPipeline()
## arguments:
## - pipeline
## description:
## - create a directory called "pipelines"
## - create a named directory for in pipelines/
## - determine running order for pipeline
## - calculate location of inputs
## - run modules in order with runModule()
runPipeline <- function(pipeline) {
    if (!file.exists("pipelines")) dir.create("pipelines")
    pipelineName <- pipeline$name
    pipelinePath <- file.path("pipelines", pipelineName)
    if (file.exists(pipelinePath))
        unlink(pipelinePath, recursive=TRUE)
    dir.create(pipelinePath, recursive=TRUE)
    pipelinePath <- file_path_as_absolute(pipelinePath)
    modules <- pipeline$components
    moduleNames <- names(modules)
    ## making a graph of the pipeline to determine order
    moduleGraph <- graphPipeline(pipeline)
    moduleOrder <- tsort(moduleGraph)
    inputs <- inputsList(pipeline$pipes, modules, pipelinePath)
    x <-
        lapply(moduleOrder,
               function (x, modules, inputs, pipelinePath) {
                   ## select inputs for this module and strip out module name
                   module <- modules[[x]]
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

## pipeStart <- function(componentName=NULL, outputName, componentRef=NULL,
##                       path=NULL) {
##     if (is.null(componentName)) {
##         list("component-ref"=componentRef, "output-name"=outputName)
##     } else {
##         list("component-name"=componentName, "output-name"=outputName)
##     }
## }

## pipeEnd <- function(componentName=NULL, inputName, componentRef=NULL,
##                       path=NULL) {
##     if (is.null(componentName)) {
##         list("component-ref"=componentRef, "input-name"=inputName)
##     } else {
##         list("component-name"=componentName, "input-name"=inputName)
##     }
## }

## ## returns a pipe list object
## pipe <- function(start, end) {
##     pipe <- list(start=start, end=end)
##     class(pipe) <- c("oapipe", "list")
##     pipe
## }

#' Create a pipe object
#'
#' @param startModuleName Name of start module
#' @param startOutputName Name of start object
#' @param endModuleName Name of end module
#' @param endInputName Name of end input
#' @param startModuleRef Address of start module
#' @param endModuleRed Address of end module
#' @return \code{pipe} connecting \code{startModuleName}.\code{startOutputName} to \code{endModuleName}.\code{endInputName}
#' @export
pipe <- function (startModuleName=NULL, startOutputName,
                  endModuleName=NULL, endInputName,
                  startModuleRef=NULL, endModuleRef=NULL) {
    start <-
        if (is.null(startModuleName)) {
            list(componentRef=startModuleRef, outputName=startOutputName)
        } else {
            list(componentName=startModuleName, outputName=startOutputName)
        }
    end <-
        if (is.null(endModuleName)) {
            list(componentRef=endModuleRef, inputName=endInputName)
        } else {
            list(componentName=endModuleName, inputName=endInputName)
        }
}


## adds a module object list to a pipeline
addModule <- function(newModule, pipeline) {
    moduleNames <- c(names(pipeline$components), newModule$name)
    pipeline$modules <- c(pipeline$components, temp=list(newModule))
    names(pipeline$components) <- moduleNames
    pipeline
}

## adds a pipe object list to a pipeline
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
pipeline <- function (name, description="", components=list(), pipes=list()) {
    names(components) <- sapply(components, function(c) { c$name })
    pipeline <- list(name=name, description=description, components=components,
                     pipes=pipes)
    class(pipeline) <- "pipeline"
    pipeline
}
