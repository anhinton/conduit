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
    names(components) <- sapply(components, getName.component)
    
    pipeline <- list(name=name, description = description,
                     components = components, pipes = pipes)
    class(pipeline) <- "pipeline"
    pipeline
}

#' @describeIn getComponents
#'
#' Returns list of \code{component} objects
#'
#' @export
getComponents.pipeline <- function(x) {
    x$components
}

#' @describeIn getName
#'
#' Returns pipeline name
#'
#' @export
getName.pipeline <- function(x) {
    x$name
}

#' @describeIn getPipes
#'
#' Returns list of \code{pipe} objects
#'
#' @export
getPipes.pipeline <- function(x) {
    x$pipes
}

#' @describeIn getDescription
#'
#' Returns pipeline description
#'
#' @export
getDescription.pipeline <- function(x) {
    x$description
}

#' @describeIn getLocation
#'
#' Returns location of pipeline XML file
#'
#' @export
getLocation.pipeline <- function(x) {
    attr(x, "location")
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
#' @param ref Path to XML file or a \code{vessel} object
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
#' pln1xml <- system.file("extdata", "simpleGraph", "pipeline.xml",
#'                        package = "conduit")
#' pln1 <- loadPipeline(name = "simpleGraph", ref = pln1xml)
loadPipeline <- function(name, ref, path = NULL,
                         namespaces=c(oa="http://www.openapi.org/2014/")) {
    ## TODO(anhinton): change how modules are loaded to include
    ## loading from URLs, files etc. The following code uses vessel
    ## objects to provide a temporary solution
    if (!inherits(ref, "vessel"))
        ref <- fileVessel(ref, path)
    ## fetch pipeline XML from disk
    rawXML <- tryCatch(
        fetchVessel(ref),
        error = function(err) {
            problem <- c(paste0("Unable to load pipeline '", name, "'\n"),
                         err)
            stop(problem)
        })
    if (!isValidXML(rawXML, "pipeline"))
        stop(paste0("'", ref, "': pipeline XML is invalid"))    
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
    names(components) <- sapply(components, getName.component)
    
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
#'
#' @import XML
pipelineToXML <- function(pipeline, namespaceDefinitions = NULL) {
    components <- getComponents(pipeline)
    pipes <- getPipes(pipeline)
    description <- getDescription(pipeline)
    pipelineRoot <- newXMLNode("pipeline",
                               namespaceDefinitions=namespaceDefinitions)
    descXML <- newXMLNode("description", description)
    componentXML <-
        lapply(X = components,
               FUN = componentToXML,
               namespaceDefinitions = namespaceDefinitions)
    pipeXML <-
        lapply(pipes, pipeToXML, namespaceDefinitions = namespaceDefinitions)
    pipelineRoot <- addChildren(
        node = pipelineRoot,
        kids = c(list(descXML), componentXML, pipeXML))
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
#' @param filename new filename
#' 
#' @return file path to resulting XML file
#' 
#' @seealso \code{pipeline}. For exporting an entire along with its
#'     components see \code{exportPipeline}
#' 
#' @examples
#' targ1 <- tempdir() 
#' ppl1xml <- system.file("extdata", "simpleGraph", "pipeline.xml", 
#' 		          package = "conduit")
#' ppl1 <- loadPipeline("simpleGraph", 
#' 		        ref = ppl1xml)
#' savePipeline(pipeline = ppl1, targetDirectory = targ1)
#'
#' @import XML
#'
#' @export
savePipeline <- function(pipeline, targetDirectory = getwd(),
                         filename = "pipeline.xml") {
    if (!file.exists(targetDirectory)) {
        stop(paste0("no such target directory: '", targetDirectory, "'"))
    }
    pipelineDoc <-
        newXMLDoc(namespaces="http://www.openapi.org/2014/",
                  node=pipelineToXML(pipeline=pipeline,
                      namespaceDefinitions="http://www.openapi.org/2014/"))
    pipelineFilePath <-
        file.path(targetDirectory, filename)
    saveXML(pipelineDoc, pipelineFilePath)
}

#' Export a pipeline and its components to disk
#'
#' Exports a \code{pipeline} and its referenced \code{component}s to
#' disk as OpenAPI XML files.
#'
#' @details Creates a directory named for the \code{pipeline} in
#'     \code{targetDirectory}, then saves \code{pipeline} and
#'     \code{component} XML files in this directory.
#'
#' The \code{pipeline} is exported as \file{pipeline.xml}, and the
#' components are exported as \file{COMPONENT_NAME.xml}.
#'
#' This functions aims to produce self-contained pipelines with all
#' components available alongside the pipeline XML file.
#'
#' @param pipeline A \code{pipeline} list
#' @param targetDirectory Output directory path
#' 
#' @return A list of the XML file paths written
#' 
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
#' pline1 <- pipeline(name = "trivialpipeline", components = list(mod1, mod2), 
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

    name <- getName(pipeline)
    description <- getDescription(pipeline)
    components <- getComponents(pipeline)
    pipes <- getPipes(pipeline)

    ## create named directory for pipeline XML files
    pipelineDirectory <- file.path(targetDirectory, name)
    if (!file.exists(pipelineDirectory)) {
        dir.create(pipelineDirectory)
    }

    ## export components to pipelineDirectory
    components <- lapply(components,
                         exportComponent,
                         targetDirectory = pipelineDirectory)

    ## rebuild pipeline
    pipeline <- pipeline(name = name, description = description,
                         components = components, pipes = pipes)
    
    ## save pipeline to xml file
    pipelineFile <- savePipeline(pipeline, pipelineDirectory)
    pipelineFile
}

## functions to run a loaded PIPELINE

#' Create an \code{input} object for executing a \code{component} in a
#' \code{pipeline}.
#'
#' @details When a pipeline is executed, conduit must match
#'     \code{moduleOutput}s to \code{moduleInput}s according to the
#'     \code{pipe}s. This function matches each \code{moduleInput}
#'     mentioned in a \code{pipe} to the correct \code{output}.
#'
#' \code{outputList} is a list of \code{output} object lists, named
#' for each component in the originating pipeline.
#'
#' @param pipe \code{pipe} describing match
#' @param outputList named list of \code{output} object lists
#'
#' @return named input list
input <- function (pipe, outputList) {
    if (!inherits(pipe, "pipe"))
        stop("pipe object required")
    startComponent <- start(pipe)$component
    startOutput <- start(pipe)$output
    componentOutputs <- outputList[[startComponent]]
    if (is.null(componentOutputs)) stop("start component does not exist")
    input <- getRef(componentOutputs[[startOutput]])
    if (is.null(input)) stop("start output does not exist")
    class(input) <- "input"
    input
}

#' Returns a named list of input objects
#'
#' \code{calculateInputs} returns a named list of \code{input} objects
#' to satisfy component inputs provided by pipes.
#' 
#' List items are named as componentName.inputName
#'
#' @param pipeList List of \code{pipe} objects
#' @param componentList List of \code{component} objects
#' @param pipelinePath Absolute file path to originating
#'     \code{pipeline} XML file
#' 
#' @return named list of lists of \code{input} objects for each
#'     component
calculateInputs <- function(pipeList, componentList, pipelinePath) {
    if (!all(sapply(pipeList, inherits, what = "pipe")))
        stop("pipeList must contain pipe objects")
    if (!all(sapply(componentList, inherits, what = "component")))
        stop("componentList must contain component objects")
    
    ## check that items in componentList are named
    if (is.null(names(componentList))) {
        names(componentList) <- sapply(componentList, getName)
    }
    
    ## calculate component output paths
    componentPaths <- lapply(componentList, componentPath, pipelinePath)

    ## resolve list of component output object lists
    outputList <- lapply(componentList,
                         function (component, componentPaths) {
                             name <- getName(component)
                             path <- getElement(componentPaths, name)
                             calculateOutputs(component, path)
                         }, componentPaths)
    
    ## match output objects to inputs named in pipeList
    inputList <- lapply(pipeList, input, outputList)
    ## name inputs in inputList for component.input from pipes
    names(inputList) <- 
        sapply(pipeList,
               function(x) {
                   paste(end(x)$component, end(x)$input, sep=".")
               })
    inputList
}

#' Get pipes as list of edges
#'
#' For each vertex (component) get a vector of vertices to which there
#' is an edge (pipe).
#'
#' @param pipeline A \code{pipeline} list object
#'
#' @return named list of character vectors for each component
pipesAsEdges <- function(pipeline) {
    if (!inherits(pipeline, "pipeline"))
        stop("pipeline object required")
    componentList <- getComponents(pipeline)
    componentNames <- sapply(componentList, getName)
    pipeList <- getPipes(pipeline)

    ## create edge list. Each component is a node, and for each node
    ## the edge list must indicate to which nodes there is an edge
    ## (pipe). All nodes must have a list of edges, even if this list
    ## is empty.
    edgeList <- sapply(
        ## for each component
        componentNames,
        function(x, pipeList) {
            ## returns names of components to which there is a pipe
            ## with the current component as startComponent
            edges <- lapply(pipeList,
                   function(y, x) {
                       if (startComponent(y) == x) {
                           endComponent(y)
                       } else {
                           character()
                       }
                   }, x)
            unlist(edges)
        }, pipeList)
    edgeList
}

#' Run a pipeline
#'
#' Executes a \code{pipeline}'s \code{component}s.
#' 
#' @details This function creates a directory named for
#'     \code{pipeline}, in a directory called \file{pipelines}, in the
#'     \code{targetDirectory}
#'     (\file{targetDirectory/pipelines/$NAME$}). If the directory
#'     \file{pipelines} does not exist in \code{targetDirectory} it
#'     will be created. Working files and output from the pipeline's
#'     \code{component}s will be stored in the named directory.
#'
#' The function returns a \code{pipelineResult} list object,
#' containing a list of \code{output} objects produced by the
#' pipeline's components.
#'
#' @param pipeline A \code{pipeline} object
#' @param targetDirectory File path for pipeline output
#' 
#' @return \code{pipelineResult} object
#' 
#' @seealso \code{pipelineResult} objects. More about \code{pipeline}
#'     objects, run single \code{module} objects with
#'     \code{runModule}.
#'
#' @examples
#' simpleGraph <-
#'     loadPipeline(name = "simpleGraph",
#'                  ref = system.file("extdata", "simpleGraph",
#'                                    "pipeline.xml",
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
#' ## bash language example
#' bashExample <- loadPipeline("bashExample",
#'                              system.file("extdata", "bashExample",
#'                                          "pipeline.xml", package="conduit"))
#' runPipeline(bashExample, targetDirectory = tempdir())
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
    if (!inherits(pipeline, "pipeline"))
        stop("pipeline object required")
    targetDirectory <- file.path(targetDirectory)
    if (!dir.exists(targetDirectory)) {
        stop("no such target directory")
    }

    componentList <- getComponents(pipeline)
    pipeList <- getPipes(pipeline)
    name <- getName(pipeline)

    ## create directory for pipeline output
    pipelinePath <- file.path(targetDirectory, "pipelines", name)
    if (dir.exists(pipelinePath))
        unlink(pipelinePath, recursive=TRUE)
    dir.create(pipelinePath, recursive=TRUE)
    pipelinePath <- normalizePath(pipelinePath)

    ## check pipeline is valid
    valid <- validatePipeline(pipeline)
    if (!valid) stop(paste0("Pipeline '", name, "' is invalid."))

    ## determine execution order of components
    edges <- pipesAsEdges(pipeline)
    componentOrder <- topologicalSort(edges)

    ## resolve inputs
    inputList <- calculateInputs(pipeList, componentList, pipelinePath)

    ## execute components in order determinde by componentOrder
    componentResultList <- lapply(
        componentList[componentOrder],
        function(component, inputList, pipelinePath) {
            name <- getName(component)                
            whichInputs <- grepl(paste0("^", name, "[.]"),
                                 names(inputList))
            inputList <- inputList[whichInputs]
            names(inputList) <-
                gsub(paste0("^", name, "[.]"), "",
                     names(inputList))
            runComponent(component, inputList, pipelinePath)
        },
        inputList,
        pipelinePath)

    ## return pipelineResult object
    pipelineResult(componentResultList, pipelinePath, pipeline)
}

## creating new pipelines

#' Add a new component to a pipeline
#'
#' This function adds a new \code{module} or \code{pipeline} to a
#' \code{pipeline}'s components.
#'
#' @param newComponent \code{pipeline} or \code{module} object to be added
#' @param pipeline \code{pipeline} to be amended
#' 
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
    pipes <- getPipes(pipeline)
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
#' pline1 <- pipeline(name = "trivialpipeline", components = list(mod1, mod2))
#' ## create a pipe
#' pipe1 <- pipe("setX", "x",
#'               "showY", "y")
#' ## add pipe to pipeline
#' pline1 <- addPipe(pipe1, pline1)
#' 
#' @export
addPipe <- function(newPipe, pipeline) {
    pipeline <- pipeline(name = getName(pipeline),
                         components = getComponents(pipeline),
                         pipes = c(getPipes(pipeline), list(newPipe)))
    pipeline
}

