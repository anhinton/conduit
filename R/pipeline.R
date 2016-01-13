#' Create a pipeline
#'
#' This functions create a new \code{pipeline} object.
#'
#' @param name \code{pipeline} name
#' @param description \code{pipeline} description
#' @param components list of \code{module} and \code{pipeline} objects
#' @param pipes list of \code{pipe} objects
#' 
#' @return \code{pipeline} list containing:
#' \item{name}{character value}
#' \item{description}{character value}
#' \item{components}{list of \code{module}s and \code{pipeline}s}
#' \item{pipes}{list of \code{pipe}s}
#'
#' @seealso \code{loadPipeline} for loading a pipeline from an XML
#'     souce, \code{module} for information on module objects,
#'     \code{runPipeline} for executing all of a pipeline's
#'     components, \code{runModule} for executing individual
#'     \code{module} objects, \code{pipe} for pipes, and
#'     \code{addPipe} and \code{addComponent} for modifying pipelines.
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
#' pline1 <- pipeline(name = "trivialpipeline", components = list(mod1, mod2))
#' ## create a pipe
#' pipe1 <- pipe("setX", "x",
#'               "showY", "y")
#' ## create a pipeline
#' pline1 <- pipeline(name = "ex_pipeline",
#'                    components = list(mod1, mod2), 
#'                    pipes = list(pipe1))
#' 
#' @export
pipeline <- function (name,
                      description = NULL,
                      components = list(),
                      pipes = list()) {
    ## check arguments for errors

    ## check 'name'
    if (!is_length1_char(name))
        stop("'name' is not a length 1 character vector")
    

    ## check 'description'
    if (!is.null(description) && !is.character(description))
        stop("'description' is not a character object")
    
    ## check 'components'
    if (!inherits(components, what = "list")) 
        stop("components must be provided in a list")
    if (!length(components)) stop("no components provided")
    if (!all(sapply(components, inherits,
                    what = c("component", "module", "pipeline"))))
        stop("components must be module or pipeline objects")
    
    ## check 'pipes'
    if (!inherits(pipes, what = "list")) 
        stop("pipes must be provided in a list")
    if (!all(sapply(pipes, inherits, what = "pipe")))
        stop("pipes must be pipe objects")    

    ## convert module and pipelines to component objects
    components <- lapply(
        components,
        function (c) {
            if(!inherits(c, "component")) {
                c <- component(name = getName(c), value = c)
            }
            c
        })
    names(components) <- sapply(components, getName)
    
    pipeline <- list(name=name, description = description,
                     components = components, pipes = pipes)
    class(pipeline) <- "pipeline"
    pipeline
}

#' @describeIn getComponents
#'
#' Returns list of \code{component} objects
getComponents.pipeline <- function(x) {
    x$components
}

#' @describeIn getName
#'
#' Returns pipeline name
getName.pipeline <- function(x) {
    x$name
}

#' @describeIn getPipes
#'
#' Returns list of \code{pipe} objects
getPipes.pipeline <- function(x) {
    x$pipes
}

#' @describeIn getDescription
#'
#' Returns pipeline description
getDescription.pipeline <- function(x) {
    x$description
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
#' 
#' @return \code{pipeline} list
#' 
#' @seealso \code{pipeline}
#' 
#' @export
#' @import XML
#' 
#' @examples
#' pln1xml <- system.file("extdata", "simpleGraph", "simpleGraph-pipeline.xml",
#'                        package = "conduit")
#' pln1 <- loadPipeline(name = "simpleGraph", ref = pln1xml)
loadPipeline <- function(name, ref, path = NULL,
                         namespaces=c(oa="http://www.openapi.org/2014/")) {
    ## fetch pipeline XML from disk
    rawXML <- tryCatch(
        fetchVessel(fileVessel(ref, path)),
        error = function(err) {
            problem <- c(paste0("Unable to load pipeline '", name, "'\n"),
                         err)
            stop(problem)
        })
    if (!isValidXML(rawXML, "pipeline"))
        stop(paste0("'", file, "': module XML is invalid"))    
    location <- attr(rawXML, "location")
    xml <- xmlRoot(xmlParse(rawXML))
    pipeline <- readPipelineXML(name, xml, location)
    pipeline
}

#' Parse pipeline XML and return a pipeline object
#'
#' @param name Pipeline name
#' @param xml Pipeline \code{XMLNode}
#' @param location file directory of invoking pipeline/module xml (optional)
#' 
#' @return \code{pipeline} object
#' 
#' @import XML
readPipelineXML <- function(name, xml, location = getwd()) {
    nodes <- xmlChildren(xml)
    namespace <- getDefaultNamespace(xml, simplify = TRUE)
    
    ## extract description
    descNode <- nodes$description
    description <- xmlValue(descNode)

    ## extract components
        
    components <- xpathApply(
        doc = xml,
        path = if (length(namespace)) {
                   "//d:component"
               } else {
                   "//component"
               },
        fun = readComponentNode,
        location = location,
        namespaces = c(d = namespace))
    names(components) <- sapply(components, getName)
    
    ## extract pipes
    pipes <- xpathApply(
        doc = xml,
        path = if (length(namespace)) {
                   "//d:pipe"
               } else {
                   "//pipe"
               },
        fun = readPipeXML,
        namespaces = c(d = namespace))

    pipeline <- pipeline(name=name, description=description,
                         components=components, pipes=pipes)
    attr(pipeline, "location") <- location
    pipeline
}

#' Parse a component \code{xmlNode} and return a \code{component}.
#'
#' @param node An \code{xmlNode} named \code{component}.
#' @param location file directory of invoking pipeline/module xml (optional)
#' 
#' @return \code{component} object
#' 
#' @import XML
readComponentNode <- function (node, location = getwd()) {
    attrs <- xmlAttrs(node)
    name <- attrs[["name"]]
    child <- xmlChildren(node)[[1]] # only one child element allowed
    childType <- names(node)[[1]]

    ## extract vessel
    vessel <- switch(
        EXPR = childType,
        file = , url = readVesselXML(child),
        NULL
    )

    ## fetch component XML
    xml <- switch(
        EXPR = childType,
        file = , url = {
            rawXML <- fetchVessel(vessel, location)
            ## update location if applicable
            if (!is.null(attr(rawXML, "location")))
                location <- attr(rawXML, "location")
            xml <- xmlRoot(xmlParse(rawXML))
            attr(xml, "location") <- attr(xml, "location")
            xml
        },
        module =, pipeline = child
    )

    ## set type
    type <- if ("type" %in% names(attrs)) {
        attrs[["type"]]
    } else {
        xmlName(xml)
    }

    ## read value from xml
    value <- switch(
        EXPR = type,
        module = readModuleXML(name = name, xml = xml, location = location),
        pipeline = readPipelineXML(name = name, xml = xml, location = location)
    )
    
    component(name = name, vessel = vessel, value = value)
}

#' Create \code{pipe} object from pipe XML
#'
#' @param node pipe node
#'
#' @return \code{pipe} object
readPipeXML <- function(node) {
    start <- xmlChildren(node)$start
    startComponent <- xmlAttrs(start)[["component"]]
    startOutput <- xmlAttrs(start)[["output"]]
    end <- xmlChildren(node)$end
    endComponent <- xmlAttrs(end)[["component"]]
    endInput <- xmlAttrs(end)[["input"]]
    pipe(startComponent=startComponent,
         startOutput=startOutput,
         endComponent=endComponent,
         endInput=endInput)
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
#' Constructor method for a \code{pipe} object which connects the
#' \code{startComponent}'s \code{startOutput} to the
#' \code{endComponent}'s \code{endInput}.
#'
#' Accessor methods are defined to extract \code{start} and \code{end}
#' elements.
#'
#' @param startComponent Name of start component
#' @param startOutput Name of start output
#' @param endComponent Name of end module
#' @param endInput Name of end input


#' Add a new component to a pipeline
#'
#' This function adds a new \code{module} or \code{pipeline} to a
#' \code{pipeline}'s components.
#'
#' @param newComponent \code{pipeline} or \code{module} object to be added
#' @param pipeline \code{pipeline} to be amended
#' @return \code{pipeline} object
#' @seealso \code{pipeline}, \code{module}
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
#' pline1 <- pipeline(name = "trivialpipeline", components = list(mod1))
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
    name <- getName(newComponent)
    oldComponents <- getComponents(pipeline)
    componentNames <- c(names(oldComponents), name)
    pipes <- getPipes(p1)
    pipeline <- pipeline(name = getName(pipeline),
                         description = getDescription(pipeline),
                         components = c(oldComponents, list(newComponent)),
                         pipes = pipes)
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

