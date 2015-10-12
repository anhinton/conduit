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
                            module = readModuleXML(name, rawValue),
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
                            module = readModuleXML(name, xml),
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
#' If the pipeline XML file is not valid OpenAPI module XML this
#' function will return an error.
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
    if (!isValidXML(file, "pipeline"))
        stop(paste0("'", file, "': module XML is invalid"))    
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
#' mod1 <- module(name = "setX", language = "R",
#'                description = "sets the value of x",
#'                outputs = list(
#'                    moduleOutput(
#'                        name = "x",
#'                        vessel = internalVessel("x"),
#'                        format = ioFormat("R character string"))),
#'                sources = list(
#'                    moduleSource(
#'                        vessel = scriptVessel("x <- \"set\""))))
#' mod2 <- module("showY", language = "R",
#'                description = "displays the value of Y",
#'                inputs = list(
#'                    moduleInput(
#'                        name = "y",
#'                        vessel = internalVessel("y"),
#'                        format = ioFormat("R character string"))),
#'                sources = list(
#'                    moduleSource(
#'                        vessel = scriptVessel("print(y)"))))
#' pline1 <- pipeline(name = "trivialpipeline", modules = list(mod1, mod2), 
#'                    pipes = list(pipe("setX", "x", "showY", "y")))
#' outputDir <- tempdir()
#'
#' ## export the pipeline to 'outputDir'
#' exportPipeline(pline1, outputDir)
#' 
#' @export
exportPipeline <- function(pipeline, targetDirectory) {
    ## stop if targetDirectory doesn't exist
    if (!file.exists(file.path(targetDirectory))) {
        stop(paste0("Target directory '", targetDirectory, "' does not exist"))
    }

    ## create named directory for pipeline XML files
    pipelineDirectory <- file.path(targetDirectory, componentName(pipeline))
    if (!file.exists(pipelineDirectory)) {
        dir.create(pipelineDirectory)
        ## FIXME: some kind of warning if I delete something?
    }

    ## give all components a ref from name
    pipeline$components <-
        lapply(pipeline$components,
               function (c) {
                   c$ref <- paste0(c$name, ".xml")
                   c$path <- NULL
                   c
               })

    ## save pipeline to xml file
    pipelineFile <- savePipeline(pipeline, pipelineDirectory)

    ## save components to XML files
    componentFiles <-
        lapply(pipeline$components,
               function (c, pipelineDirectory) {
                   exportComponent(c, pipelineDirectory)
               }, pipelineDirectory)

    c(pipeline = pipelineFile, componentFiles)
}

## functions to run a loaded PIPELINE

#' Match a pipe's input name to an output object
#'
#' @param pipe \code{pipe} describing match
#' @param outputObjects named list of output objects
#'
#' @return named input list
matchInput <- function (pipe, outputObjects) {
    component <- getElement(outputObjects, pipe$start$component)
    object <- getElement(component, pipe$start$output)
    return(object)
}

#' Returns a named list of input objects
#'
#' \code{inputsList} returns a named list of absolute file locations for
#' components' inputs.
#' 
#' List items are named as COMPONENT_NAME.INPUT_NAME
#'
#' @param pipes List of \code{pipe}s
#' @param components List of \code{component}s
#' @param pipelinePath Absolute file path to originating \code{pipeline}
#' XML file
#' 
#' @return named list of file locations by input names
inputObjects <- function(pipes, components, pipelinePath) {
    ## calculate component output paths
    componentPaths <- lapply(components, componentPath, pipelinePath)

    ## extract actual objects
    componentValues <-
        lapply(
            components,
            function(component) {
                value <- component$value
                return(value)
            })                            

    ## calculate component output objects
    outputObjects <- lapply(componentValues,
           function (value, componentPaths) {
               path <- getElement(componentPaths, value$name)
               output <- calculateOutputs(value, path)
           }, componentPaths)

    ## match output objects to input names
    inputObjects <- lapply(pipes, matchInput, outputObjects)
    names(inputObjects) <- 
        sapply(pipes,
               function(x) {
                   paste(x$end$component, x$end$input, sep=".")
               })
    return(inputObjects)
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
#' @details This function creates a directory called
#' \code{pipeline$name} in \file{pipelines}, in the
#' \code{targetDirectory}. If the directory \file{pipelines} does not
#' exist in this directory it will be created. Working files and
#' output from the pipeline's \code{components} will be stored in the
#' named directory.
#'
#' First the function will determine the order in which its components are to
#' be run. Note that the \code{pipeline} is not allowed to have any cycles or
#' the function will fail.
#'
#' The function then produces a list of inputs required by the
#' \code{component}s, and resolves the (intended) location of these.
#'
#' Finally the function executes each \code{component} in the 
#' determined order.
#'
#' @param pipeline A \code{pipeline} object
#' @param targetDirectory File path for pipeline output
#' 
#' @return Named list of \code{component} output objects
#' 
#' @seealso More about \code{pipeline} objects, run single \code{module}
#' objects with \code{runModule}.
#'
#' @examples
#' simpleGraph <-
#'     loadPipeline(name = "simpleGraph",
#'                  ref = system.file("extdata", "simpleGraph",
#'                                    "simpleGraph-pipeline.xml",
#'                                     package = "conduit"))
#'
#' ## run the pipeline
#' runPipeline(simpleGraph, targetDirectory = tempdir())
#'
#' ## python language example
#' pythonExample <- loadPipeline("pythonExample",
#'                               system.file("extdata", "pythonExample",
#'                                           "pipeline.xml", package="conduit"))
#' runPipeline(pythonExample, targetDirectory = tempdir())
#'
#' ## shell language example
#' shellExample <- loadPipeline("shellExample",
#'                              system.file("extdata", "shellExample",
#'                                          "pipeline.xml", package="conduit"))
#' runPipeline(shellExample, targetDirectory = tempdir())
#'
#' ## A pipeline with a module run on a remote host
#' \dontrun{
#'   irisplots_host_pipeline <- loadPipeline(
#'       "irisplots_host_pipeline",
#'       system.file("extdata", "irisplots_host", "pipeline.xml",
#'       package = "conduit"))
#'   runPipeline(irisplots_host_pipeline, targetDirectory = tempdir())
#' }
#' 
#' @export
runPipeline <- function(pipeline, targetDirectory = getwd()) {
    ## ensure targetDirectory exists
    targetDirectory <- file.path(targetDirectory)
    if (!file.exists(targetDirectory)) {
        stop("no such target directory")
    }
    
    ## create directory for pipeline output
    targetDirectory <- file.path(targetDirectory, "pipelines")
    if (!file.exists(targetDirectory)) {
        dir.create(targetDirectory)
    }
    pipelineName <- componentName(pipeline)
    pipelinePath <- file.path(targetDirectory, pipelineName)
    if (file.exists(pipelinePath))
        unlink(pipelinePath, recursive=TRUE)
    dir.create(pipelinePath, recursive=TRUE)
    ## inputs will need the full file path
    pipelinePath <- normalizePath(pipelinePath)

    ## validate pipes
    valid <- validatePipeline(pipeline)
    if (!valid) stop(paste0("Pipeline '", pipelineName, "' is invalid."))

    ## make a graph of the pipeline to determine order
    componentGraph <- graphPipeline(pipeline)
    componentOrder <- RBGL::tsort(componentGraph)

    ## resolve inputs
    inputObjects <- inputObjects(pipes = pipeline$pipes,
                                 components = pipeline$components,
                                 pipelinePath = pipelinePath)

    ## execute components in order determinde by componentOrder
    outputObjects <-
        lapply(
            componentOrder,
            function(componentName, pipeline, inputObjects, pipelinePath) {
                whichInputs <- grepl(paste0("^", componentName, "[.]"),
                                     names(inputObjects))
                inputObjects <- inputObjects[whichInputs]
                names(inputObjects) <-
                    gsub(paste0("^", componentName, "[.]"), "",
                         names(inputObjects))
                outputObjects <- runComponent(componentName, pipeline,
                                              inputObjects, pipelinePath)
                return(outputObjects)
            }, pipeline, inputObjects, pipelinePath)
    names(outputObjects) <- componentOrder
    return(outputObjects)
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
#' mod1 <- module(name = "setX", language = "R",
#'                description = "sets the value of x",
#'                outputs = list(
#'                    moduleOutput(
#'                        name = "x",
#'                        vessel = internalVessel("x"),
#'                        format = ioFormat("R character string"))),
#'                sources = list(
#'                    moduleSource(
#'                        vessel = scriptVessel("x <- \"set\""))))
#' pline1 <- pipeline(name = "trivialpipeline", modules = list(mod1))
#' ## create a new module
#' mod2 <- module("showY", language = "R",
#'                description = "displays the value of Y",
#'                inputs = list(
#'                    moduleInput(
#'                        name = "y",
#'                        vessel = internalVessel("y"),
#'                        format = ioFormat("R character string"))),
#'                sources = list(
#'                    moduleSource(
#'                        vessel = scriptVessel("print(y)"))))
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
#' mod1 <- module(name = "setX", language = "R",
#'                description = "sets the value of x",
#'                outputs = list(
#'                    moduleOutput(
#'                        name = "x",
#'                        vessel = internalVessel("x"),
#'                        format = ioFormat("R character string"))),
#'                sources = list(
#'                    moduleSource(
#'                        vessel = scriptVessel("x <- \"set\""))))
#' mod2 <- module("showY", language = "R",
#'                description = "displays the value of Y",
#'                inputs = list(
#'                    moduleInput(
#'                        name = "y",
#'                        vessel = internalVessel("y"),
#'                        format = ioFormat("R character string"))),
#'                sources = list(
#'                    moduleSource(
#'                        vessel = scriptVessel("print(y)"))))
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
#' @seealso \code{loadPipeline} for loading a pipeline from an XML
#' souce, \code{module} for information on module objects,
#' \code{runPipeline} for executing all of a pipeline's components,
#' \code{runModule} for executing individual \code{module} objects,
#' \code{pipe} for pipes, and \code{addPipe} and \code{addComponent}
#' for modifying pipelines.
#'
#' @examples
#' ## create some modules
#' mod1 <- module(name = "setX", language = "R",
#'                description = "sets the value of x",
#'                outputs = list(
#'                    moduleOutput(
#'                        name = "x",
#'                        vessel = internalVessel("x"),
#'                        format = ioFormat("R character string"))),
#'                sources = list(
#'                    moduleSource(
#'                        vessel = scriptVessel("x <- \"set\""))))
#' mod2 <- module("showY", language = "R",
#'                description = "displays the value of Y",
#'                inputs = list(
#'                    moduleInput(
#'                        name = "y",
#'                        vessel = internalVessel("y"),
#'                        format = ioFormat("R character string"))),
#'                sources = list(
#'                    moduleSource(
#'                        vessel = scriptVessel("print(y)"))))
#' pline1 <- pipeline(name = "trivialpipeline", modules = list(mod1, mod2))
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
