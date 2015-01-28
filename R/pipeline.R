### load, save, run and create pipelines

#' Parse a component \code{xmlNode} and return a \code{component}.
#'
#' @param node An \code{xmlNode} named \dQuote{component}.
#' @param location file directory of invoking pipeline/module xml (optional)
#' @return \code{component} object
#' @import XML
readComponentNode <- function (node, location) {
    name <- getXMLAttr(node, "name")
    ref <- getXMLAttr(node, "ref")
    component <-
        if (is.null(ref)) {
            rawValue <- xmlChildren(node)[[1]] # only first component node
                                               # counts
            ## check if the component has any content
            if (is.null(rawValue)) {
                stop(paste("Component", name, "is empty!"))
            }
            type <- names(node)[[1]]
            ## check for appropriate types
            if (type != "module" && type != "pipeline") {
                stop("A component must be a module or a pipeline")
            }
            value <- switch(type,
                            module = readModuleXML(name, rawValue,
                                                   location),
                            pipeline = readPipelineXML(name, rawValue,
                                                       location))
            component(name = name, type = type, value = value)
        } else {
            path <- getXMLAttr(node, "path")
            ## ## if a path is not given assume this means the xml file
            ## ## is found in the same directory as the pipeline xml
            ## if (is.null(path)) path <- pipelinePath
            type <- getXMLAttr(node, "type")
            ## if type is not 'module' or 'pipeline' then something is wrong
            if (type != "module" && type != "pipeline") {
                stop("A component must be a module or a pipeline")
            }
            file <- tryCatch(
                resolveRef(ref, path, location),
                error = function(err) {
                    problem <-
                        c(paste0("Unable to locate component '", name, "'\n"),
                          err)
                    stop(problem)
                })
            location <- dirname(file)
            rawXML <- fetchRef(file)
            xml <- xmlRoot(xmlParse(rawXML))
            value <- switch(type,
                            module = readModuleXML(name, xml, location),
                            ## FIXME: I bet loading a pipeline won't work
                            pipeline = readPipelineXML(name, xml, location))
            component(name=name, ref=ref, path=path, type=type, value=value)
            ## FIXME: can't handle anon/inline components
        }
    ## path <-
    ## FIXME: need to check whether module or pipeline
    component
}

#' Parse pipeline XML and return a pipeline object
#'
#' @param name Pipeline name
#' @param xml Pipeline \code{XMLNode}
#' @param location file directory of invoking pipeline/module xml (optional)
#' @return \code{pipeline} object
#' @import XML
readPipelineXML <- function(name, xml, location = getwd()) {
    ## pipelinePath <- paste0(pipelineDir, pathSep)
    nodes <- xmlChildren(xml)
    
    ## extract description
    descNode <- nodes$description
    description <- xmlValue(descNode)

    ## extract components
    componentNodes <- nodes[names(nodes) == "component"]
    components <- lapply(componentNodes, readComponentNode, location)
    names(components) <-
        sapply(components, componentName)
    ## FIXME: need to address inline components
    
    ## extract pipes
    pipeNodes <- nodes[names(nodes) == "pipe"]
    pipes <-
        lapply(pipeNodes,
               function (x) {
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
               })    
    pipeline(name=name, description=description,
             components=components, pipes=pipes)
}

#' Load a pipeline from an XML file
#'
#' Reads an XML file given by \code{ref} and \code{path} and interprets to
#' produce a \code{pipeline}.
#'
#' If \code{path} is not set and conduit needs to search for the file the
#' default search paths are used.
#' 
#' @param name Name of pipeline
#' @param ref Path to XML file
#' @param path Search path (optional)
#' @param namespaces Namespaces used in XML document
#' @return \code{pipeline} list
#' @seealso \code{pipeline}
#' @export
#' @import XML
#' @examples
#' pln1xml <- system.file("extdata", "simpleGraph", "simpleGraph-pipeline.xml",
#'                        package = "conduit")
#' pln1 <- loadPipeline(name = "simpleGraph", ref = pln1xml)
loadPipeline <- function(name, ref, path = NULL,
                         namespaces=c(oa="http://www.openapi.org/2014/")) {
    ## fetch pipeline XML from disk
    file <- tryCatch(
        resolveRef(ref, path),
        error = function(err) {
            problem <- c(paste0("Unable to load module '", name, "'\n"),
                         err)
            stop(problem)
        })
    location <- dirname(file)
    rawXML <- fetchRef(file)
    xml <- xmlRoot(xmlParse(rawXML))
    pipeline <- readPipelineXML(name, xml, location)
    pipeline
}

## functions to write a pipeline (and its modules) to XML files

#' Convert a pipeline to XML
#'
#' @param pipeline \code{pipeline} object
#' @param namespaceDefinitions XML namespaces as character vector
#' 
#' @return \code{XMLNode} object
pipelineToXML <- function(pipeline, namespaceDefinitions=NULL) {
    pipelineRoot <- newXMLNode("pipeline",
                               namespaceDefinitions=namespaceDefinitions)
    description <- newXMLNode("description", pipeline$description)
    components <-
        lapply(pipeline$components,
               function(c) {
                   componentRoot <-
                       newXMLNode("component",
                                  attrs=c(name=c$name))
                   if (is.null(c$ref)) {
                       componentXML <- componentToXML(c)
                       componentRoot <-
                           addChildren(componentRoot,
                                       kids=list(componentXML))
                   } else {
                       xmlAttrs(componentRoot) <-
                           c(ref  = c$ref,
                             path = c$path,
                             type = c$type)
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
#' As at 2014-08-12 the resulting file is always called \file{pipeline.xml}
#'
#' @param pipeline \code{pipeline} object
#' @param targetDirectory file location to save output
#' @return file path to resulting XML file
#' @seealso \code{pipeline}. For exporting a pipeline and its components see
#' \code{exportPipeline}
#' @examples
#' targ1 <- tempdir() 
#' ppl1xml <- system.file("extdata", "simpleGraph", "simpleGraph-pipeline.xml", 
#' 		          package = "conduit")
#' ppl1 <- loadPipeline("simpleGraph", 
#' 		        ref = ppl1xml)
#' savePipeline(pipeline = ppl1, targetDirectory = targ1)
#' @import XML
#' @export
savePipeline <- function(pipeline, targetDirectory=getwd()) {
    if (!file.exists(targetDirectory)) {
        stop(paste0("no such target directory: '", targetDirectory, "'"))
    }
    pipelineDoc <-
        newXMLDoc(namespaces="http://www.openapi.org/2014",
                  node=pipelineToXML(pipeline=pipeline,
                      namespaceDefinitions="http://www.openapi.org/2014/"))
    pipelineFilePath <-
        file.path(targetDirectory,
                  "pipeline.xml")
    saveXML(pipelineDoc, pipelineFilePath)
}

#' Export a pipeline and its components to disk
#'
#' Exports a \code{pipeline} and its referenced \code{component}s to disk as
#' openapi XML files.
#'
#' @details Creates a directory named for the \code{pipeline$name} in
#' \code{targetDirectory}, then saves \code{pipeline} and \code{component} XML
#' files in this directory.
#'
#' As at 2014-08-12 the \code{pipeline} is always saved as \file{pipeline.xml}
#' no matter what the \code{pipeline} name.
#'
#' @param pipeline A \code{pipeline} list
#' @param targetDirectory Output directory path
#' @return A list of the XML file paths written
#' @seealso \code{pipeline}, \code{savePipeline}
#'
#' @examples
#' ## create a pipeline
#' mod1 <- module(name = "setX", platform = "R",
#'                description = "sets the value of x",
#'                outputs = list(moduleOutput(name = "x", type = "internal",
#' 					   format = "R character string")),
#'                sources = list(moduleSource(value = "x <- \"set\"")))
#' mod2 <- module("showY", platform = "R",
#'                description = "displays the value of Y",
#'                inputs = list(moduleInput(name = "y", type = "internal",
#'                                          format = "R character string")),
#'                sources = list(moduleSource(value = "print(y)")))
#' pline1 <- pipeline(name = "trivialpipeline", modules = list(mod1, mod2), 
#'                    pipes = list(pipe("setX", "x", "showY", "y")))
#' outputDir <- tempdir()
#'
#' ## export the pipeline to 'outputDir'
#' exportPipeline(pline1, outputDir)
#' 
#' @export
exportPipeline <- function(pipeline, targetDirectory) {
    if (!file.exists(file.path(targetDirectory))) {
        stop(paste0("Target directory '", targetDirectory, "' does not exist"))
    }
    pipelineDirectory <- file.path(targetDirectory, componentName(pipeline))
    if (!file.exists(pipelineDirectory)) {
        dir.create(pipelineDirectory)
    ## ## FIXME: I was producing this warning every time AKA ignoring it, so it
    ## ## would be far better if I got some real warnings and errors and what
    ## ## have you in place
    ## } else {
    ##     warning("this pipeline directory exists and you might be writing over something useful")
    }
    pipelineFile <- savePipeline(pipeline, pipelineDirectory)
    ## determine which components have a `ref`
    componentHasRef <-
        sapply(pipeline$components,
               function (c) {
                   hasRef <- if (!is.null(c$ref)) {
                       TRUE
                   } else {
                       FALSE
                   }
                   hasRef
               })
    ## export components with `ref` to XML
    result <-
        lapply(which(componentHasRef),
               function (n, components, pipelineDirectory) {
                   component <- components[[n]]
                   exportComponent(component, pipelineDirectory)
               }, pipeline$components, pipelineDirectory)
    result <- c(pipeline = pipelineFile, result)
    result
}

## functions to run a loaded PIPELINE

#' Returns the correct file extension for a platform's 'internal' files
#'
#' @param platform platform name
#' @return files exension as character as ".EXT"
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
#' @param pipelinePath Absolute file path to originatin \code{pipeline}
#' XML file
#' @return named list of file locations by input names
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
                           findFile(ref)
                       }
                   }
                   input
               }, components, pipelinePath)
    names(inputsList) <- inputNames
    inputsList
}

#' Create a \code{graphNEL} node-and-edge graph of a pipeline
#'
#' \code{graphPipeline} produces a directed graph of the given
#' \code{pipeline} with components as nodes and pipes as directed edges
#'
#' @param pipeline A \code{pipeline} list object
#'
#' @return A \pkg{graph} \code{graphNEL} object
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
    pipelineGraph <- graph::graphNEL(nodes=componentNames, edgeL=edgeList,
                                     edgemode="directed")
    pipelineGraph
}

#' Run a pipeline
#'
#' Executes a \code{pipeline}'s \code{component}s.
#' 
#' @details This function creates a directory called \code{pipeline$name} in
#' \file{pipelines}, in the working directory. If the directory \file{pipelines}
#' does not exist in the working directory it will be created. Working files
#' and output from the pipeline's \code{components} will be stored in the
#' named directory.
#'
#' First the function will determine the order in which its components are to
#' be run. Note that the \code{pipeline} is not allowed to have any cycles or
#' the function will fail.
#'
#' The function then produces a list of inputs required by the
#' \code{component}s, and resolves the (intended) location of these.
#'
#' Finally the function executes each \code{component}, in the order determined,
#' by passing each component and the inputs list to \code{runComponent}.
#'
#' @param pipeline A \code{pipeline} object
#' @return Meaningless list. TODO: fix what \code{runPipeline},
#' \code{runModule}, \code{runPlatform} return.
#' @seealso \code{pipeline}, \code{runComponent}
#'
#' @examples
#' simpleGraph <-
#'     loadPipeline(name = "simpleGraph",
#'                  ref = system.file("extdata", "simpleGraph",
#'                                    "simpleGraph-pipeline.xml",
#'                                     package = "conduit"))
#' ## run example in temp directory
#' oldwd <- setwd(tempdir())
#'
#' ## run the pipeline
#' runPipeline(simpleGraph)
#' ## observe results
#' list.files(file.path(tempdir(), "pipelines"), recursive = TRUE)
#' setwd(oldwd)
#' 
#' @export
runPipeline <- function(pipeline) {
    ## create directory for pipeline output
    if (!file.exists("pipelines")) dir.create("pipelines")
    pipelineName <- componentName(pipeline)
    pipelinePath <- file.path("pipelines", pipelineName)
    pipelinePath <- normalizePath(pipelinePath)
    if (file.exists(pipelinePath))
        unlink(pipelinePath, recursive=TRUE)
    dir.create(pipelinePath, recursive=TRUE)

    ## validate pipes
    valid <- validatePipeline(pipeline)
    if (!valid) stop(paste0("Pipeline '", pipelineName, "' is invalid."))

    ## make a graph of the pipeline to determine order
    componentGraph <- graphPipeline(pipeline)
    componentOrder <- RBGL::tsort(componentGraph)

    ## resolve inputs
    inputs <- inputsList(pipeline$pipes, pipeline$components,
                         pipelinePath)

    ## execute components in order determined by componentOrder
    x <-
        lapply(componentOrder,
               function (x, pipeline, inputs, pipelinePath) {
                   ## select inputs for this component and strip out
                   ## component name

                   ## FIXME: selecting inputs from inputsList seems a little
                   ## inelegant. Possibly calculating all input locations
                   ## before anything is run is the reason for this.
                   ## What else shall we try?

                   ## FIXME: this could be a helper function called something
                   ## like resolveComponentInputs()
                   whichInputs <-
                       grepl(paste0("^", x,"[.]"),
                             names(inputs))
                   inputs <- inputs[whichInputs]
                   names(inputs) <-
                       gsub(paste0("^", x,"[.]"), "",
                            names(inputs))
                   
                   ## run the beast
                   runComponent(x, pipeline, inputs, pipelinePath)
               }, pipeline, inputs, pipelinePath)
}

## creating new pipelines

#' Creates a \code{pipe} object
#'
#' Creates a \code{pipe} object which connects the \code{startComponent}'s
#' \code{startOutput} to the \code{endComponent}'s \code{endInput}.
#'
#' @param startComponent Name of start component
#' @param startOutput Name of start output
#' @param endComponent Name of end module
#' @param endInput Name of end input
#' @return \code{pipe} connecting \code{startComponentName}.\code{startOutputName} to \code{endComponentName}.\code{endInputName}
#' @seealso \code{pipeline}, \code{addPipe}
#'
#' @examples
#' pipe1 <- pipe(startComponent = "setX", startOutput = "x",
#'               endComponent = "showY", endInput = "y")
#' 
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
#' This function adds a new \code{component} to a \code{pipeline}.
#'
#' @param newComponent \code{pipeline} or \code{module} object to be added
#' @param pipeline \code{pipeline} to be amended
#' @return \code{pipeline} object
#' @seealso \code{pipeline}, \code{component}, \code{module}
#'
#' @examples
#' ## create a pipeline with one module
#' mod1 <- module(name = "setX", platform = "R",
#'                description = "sets the value of x",
#'                outputs = list(moduleOutput(name = "x", type = "internal",
#' 					   format = "R character string")),
#'                sources = list(moduleSource(value = "x <- \"set\"")))
#' pline1 <- pipeline(name = "trivialpipeline", modules = list(mod1))
#' ## create a new module
#' mod2 <- module("showY", platform = "R",
#'                description = "displays the value of Y",
#'                inputs = list(moduleInput(name = "y", type = "internal",
#'                                          format = "R character string")),
#'                sources = list(moduleSource(value = "print(y)")))
#' ## add new module to pipeline
#' pline1 <- addComponent(mod2, pline1)
#' 
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

#' Add a pipe to a pipeline
#'
#' This functions adds a new \code{pipe} to a \code{pipeline}.
#'
#' @param newPipe \code{pipe} object
#' @param pipeline \code{pipeline} object
#' @return \code{pipeline} object
#' @seealso \code{pipe}, \code{pipeline}
#'
#' @examples
#' ## create a pipeline with two modules
#' mod1 <- module(name = "setX", platform = "R",
#'                description = "sets the value of x",
#'                outputs = list(moduleOutput(name = "x", type = "internal",
#' 					   format = "R character string")),
#'                sources = list(moduleSource(value = "x <- \"set\"")))
#' mod2 <- module("showY", platform = "R",
#'                description = "displays the value of Y",
#'                inputs = list(moduleInput(name = "y", type = "internal",
#'                                          format = "R character string")),
#'                sources = list(moduleSource(value = "print(y)")))
#' pline1 <- pipeline(name = "trivialpipeline", modules = list(mod1, mod2))
#' ## create a pipe
#' pipe1 <- pipe("setX", "x",
#'               "showY", "y")
#' ## add pipe to pipeline
#' pline1 <- addPipe(pipe1, pline1)
#' 
#' @export
addPipe <- function(newPipe, pipeline) {
    pipeline$pipes <- c(pipeline$pipes, list(newPipe))
    pipeline
}

#' return the name of a component
#'
#' Returns the name of a \code{module} or \code{pipeline}
#'
#' @param component \code{module} or \code{pipeline} object
#' @return character value
componentName <- function (component) {
    component$name
}

#' Create a pipeline
#'
#' This functions create a new \code{pipeline} object.
#'
#' @details If \code{components} is empty the \code{modules} and
#' \code{pipelines} arguments will be used to create the pipeline.
#'
#' @param name \code{pipeline} name
#' @param description \code{pipeline} description
#' @param components list of \code{module} and \code{pipeline} objects
#' @param modules list of \code{module} objects
#' @param pipelines list of \code{pipeline} objects
#' @param pipes list of \code{pipe} objects
#' @return \code{pipeline} list containing:
#' \item{name}{character value}
#' \item{description}{character value}
#' \item{components}{list of \code{module}s and \code{pipeline}s}
#' \item{pipes}{list of \code{pipe}s}
#' @seealso \code{loadPipeline} for loading a pipeline from an XML souce,
#' \code{module} for information on module objects, \code{runPipeline} for
#' executing all of a pipeline's components, \code{runComponent} for executing
#' individual components, \code{pipe} for pipes, and \code{addPipe} and
#' \code{addComponent} for modifying pipelines.
#'
#' @examples
#' ## create some modules
#' mod1 <- module(name = "setX", platform = "R",
#'                description = "sets the value of x",
#'                outputs = list(moduleOutput(name = "x", type = "internal",
#' 					   format = "R character string")),
#'                sources = list(moduleSource(value = "x <- \"set\"")))
#' mod2 <- module("showY", platform = "R",
#'                description = "displays the value of Y",
#'                inputs = list(moduleInput(name = "y", type = "internal",
#'                                          format = "R character string")),
#'                sources = list(moduleSource(value = "print(y)")))
#' ## create a pipe
#' pipe1 <- pipe("setX", "x",
#'               "showY", "y")
#' ## create a pipeline
#' pline1 <- pipeline(name = "ex_pipeline",
#'                    modules = list(mod1, mod2), 
#'                    pipes = list(pipe1))
#' 
#' @export
pipeline <- function (name, description="", components=list(),
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
                component <- component(name = c$name, value = c, path = c$pathq)
            }
            component
        })
    pipeline <- list(name=name, description=description,
                     components=components, pipes=pipes)
    class(pipeline) <- "pipeline"
    pipeline
}
