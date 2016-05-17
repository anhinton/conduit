#' Create a component object
#'
#' Create a \code{component} object for use in a \code{pipeline}.
#'
#' @details A component contains either a \code{pipeline} or
#'     \code{module} object in its \code{value} output. Optionally it
#'     can contain a \code{fileVessel} or \code{urlVessel} in its
#'     \code{vessel} element, referencing the resource from which the
#'     component was loaded.
#'
#' @param value \code{pipeline} or \code{module} object
#' @param name Name of component
#' @param vessel \code{fileVessel} or \code{urlVessel}
#' 
#' @return \code{component} list containing:
#'   \item{name}{component name}
#'   \item{vessel}{source vessel}
#'   \item{value}{\code{pipeline} or \code{module} object}
#' 
#' @seealso \code{pipeline}, \code{module}
#'
#' @export
component <- function(value,
                      name = NULL,
                      vessel = NULL) {
    if (is.null(name)) name <- getName(value)
    if (!is_length1_char(name)) stop("'name' is not a length 1 char value")
    if (!inherits(value, c("module", "pipeline")))
        stop("invalid 'value'")
    if (!is.null(vessel) &&
        !inherits(vessel, what = c("fileVessel", "urlVessel"))) {
        stop("invalid 'vessel'")
    }
    
    component <- list(name = name, vessel = vessel, value = value)
    class(component) <- "component"
    component
}

#' @describeIn getName
#'
#' Returns component name
#'
#' @export
getName.component <- function (x) {
    x$name
}

#' @describeIn getType
#'
#' Returns component type
#'
#' @export
getType.component <- function(x) {
    class(getValue(x))
}

#' @describeIn getVessel
#'
#' Returns component vessel
#'
#' @export
getVessel.component <- function(x) {
    x$vessel
}

#' @describeIn getValue
#'
#' Returns module or pipeline object
#'
#' @export
getValue.component <- function(x) {
    x$value
}

### Functions for exporting, running and creating components

#' Convert a component to XML
#'
#' Convert a \code{component} object into the corresponding openapi XML
#' format.
#'
#' @param component \code{component} object
#' @param namespaceDefinitions As named character vector
#' 
#' @return \code{xmlNode} object
#'
#' @import XML
componentToXML <- function(component, namespaceDefinitions = NULL) {
    name <- getName(component)
    vessel <- getVessel(component)
    type <- getType(component)
    value <- getValue(component)    
    componentRoot <- newXMLNode("component", attrs = c(name = name))
    valueXML <-
        if(is.null(vessel)) {
            switch(
                type,
                module = moduleToXML(value, namespaceDefinitions),
                pipeline = pipelineToXML(value, namespaceDefinitions))
        } else {
            vesselToXML(vessel)
        }
    componentRoot <- addChildren(componentRoot, kids = list(valueXML))
    if (!is.null(vessel))
        xmlAttrs(componentRoot, append = TRUE) <-  c(type = type)
    componentRoot
}

#' Export a component to an XML file
#'
#' Saves an XML of the component in the
#' \code{targetDirectory}. Returns a component object to be used by a
#' \code{pipeline} XML file also in \code{targetDirectory}.
#'
#' @param component \code{component} object
#' @param targetDirectory File path for pipeline output
#' 
#' @return \code{component} object
exportComponent <- function(component, targetDirectory = getwd()) {
    ## stop of targetDirectory doesn't exist
    if (!file.exists(targetDirectory)) {
        stop("no such target directory")
    }

    name <- getName(component)
    value <- getValue(component)
    vessel <- getVessel(component)
    type <- getType(component)

    ## save referenced vessel to targetDirectory
    if (inherits(vessel, "fileVessel")) {
        fullPath <- findFile(vessel$ref, vessel$path, getLocation(value))
        oldFilename <- basename(fullPath)
        newFilename <- paste0(name, ".xml")
        newPath <- file.path(targetDirectory, newFilename)
        file.copy(fullPath, newPath)
        vessel <- fileVessel(ref = newFilename)
    } else if (is.null(vessel)) {
        file <- switch(type,
                       module = saveModule(value,
                                           targetDirectory,
                                           paste0(name, ".xml")),
                       pipeline = savePipeline(value,
                                               targetDirectory,
                                               paste0(name, ".xml")))
        vessel <- fileVessel(ref = basename(file))
    }

    ## update component object with fileVessel created in targetDirectory
    component <- component(vessel = vessel, value = value)
    component
}

#' Calculate \code{output} objects produced by a \code{module}
#'
#' @param module \code{module} object
#' @param outputDirectory file location for outputs
#'
#' @return list of \code{output} objects
calculateModuleOutputs <- function(module, outputDirectory) {
    moduleLanguage <- getLanguage(module)
    outputs <- lapply(module$outputs, output, getLanguage(moduleLanguage),
                      outputDirectory)
    outputs
}

#' Calculate ouput objects produced by a pipeline \code{component}
#' 
#' @details As at 2016-01-19 a method for \code{pipeline} objects has
#'     not been implemented.
#'
#' @param component \code{component} object
#' @param outputDirectory file location for component outputs
#'
#' @return named list of \code{output} objects
calculateOutputs <- function(component, outputDirectory) {
    if (!inherits(component, "component"))
        stop("component object required")
    type <- getType(component)
    value <- getValue(component)    
    outputList <- switch(
        type,
        module = calculateModuleOutputs(value, outputDirectory),
        stop("component type not supported"))
    outputList
}

#' Calculate a component's output path
#'
#' @param component \code{component} object
#' @param pipelinePath output path of parent pipeline
#'
#' @return output path as character
componentPath <- function (component, pipelinePath) {
    if (!inherits(component, "component"))
        stop("component object require")
    name <- getName(component)
    file.path(pipelinePath, name)
}

#' Run a component
#'
#' This function executes a single pipeline component with \code{runModule} or
#' \code{runPipeline}.
#'
#' @details If the component refers to a \code{module}, the names of
#'     the elements in \code{inputList} must match the names the
#'     module's input names.
#'
#' \code{pipelinePath} must exist on the filesystem.
#'
#' @param component \code{component} to be executed
#' @param inputList Named list of \code{input} objects
#' @param pipelinePath Pipeline output directory
#' 
#' @return \code{componentResult} object
runComponent <- function(component, inputList = list(),
                         pipelinePath = getwd()) {
    if (!inherits(component, "component"))
        stop("component object required")
    value <- getValue(component)
    type <- getType(component)
    switch(
        type,
        module = runModule(module = value,
                           targetDirectory = pipelinePath,
                           inputObjects = inputList),
        ## FIXME: running pipelines probably doesn't work
        pipeline = runPipeline(value))
}
