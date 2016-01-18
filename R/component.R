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
#' @param name Name of component
#' @param vessel \code{fileVessel} or \code{urlVessel}
#' @param value \code{pipeline} or \code{module} object
#' 
#' @return \code{component} list containing:
#'   \item{name}{component name}
#'   \item{vessel}{source vessel}
#'   \item{value}{\code{pipeline} or \code{module} object}
#' 
#' @seealso \code{pipeline}, \code{module}
#'
#' @export
component <- function(name,
                      vessel = NULL,
                      value) {
    if (missing(name)) name <- getName(value)
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
getName.component <- function (x) {
    x$name
}

#' @describeIn getType
#'
#' Returns component type
getType.component <- function(x) {
    class(getValue(x))
}

#' @describeIn getVessel
#'
#' Returns component vessel
getVessel.component <- function(x) {
    x$vessel
}

#' @describeIn getValue
#'
#' Returns module or pipeline object
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

#' calculate output objects produced by a module
#'
#' @param componentValue \code{module} object from \code{component}
#'     \code{value} slot
#' @param outputDirectory file location for pipeline execution output
calculateOutputs.module <- function(componentValue, outputDirectory) {
    language <- componentValue$language
    outputObjects <- lapply(componentValue$outputs, outputObject, language,
                            outputDirectory)
    return(outputObjects)
}

#' Calculate ouput objects produced by a component
#'
#' @param componentValue \code{module} or \code{pipeline} object
#' @param outputDirectory file location for component outputs
#'
#' @return named list of output objects
calculateOutputs <- function(componentValue, outputDirectory) {
    UseMethod("calculateOutputs")
}

#' Calculate a component's output path
#'
#' @param component \code{component} object
#' @param pipelinePath output path of parent pipeline
#'
#' @return output path as character
componentPath <- function (component, pipelinePath) {
    path <-
        switch(
            class(component$value),
            module = file.path(pipelinePath, "modules", component$name),
            stop("Unknown component type")
        )
    return(path)
}

#' Run a component
#'
#' This function executes a single pipeline component with \code{runModule} or
#' \code{runPipeline}.
#'
#' @details If the component refers to a \code{module}, the names of
#' \code{inputs} must match the names the module's input names.
#'
#' \code{pipelinePath} must exist on the filesystem.
#'
#' @param componentName Name of component to be executed
#' @param pipeline \code{pipeline} containing component
#' @param inputObjects Named list of input objects
#' @param pipelinePath Pipeline output directory
#' 
#' @return Named list of output objects
runComponent <- function(componentName, pipeline, inputObjects = list(),
                         pipelinePath=getwd()) {
    component <- pipeline$components[[componentName]]
    value <- component$value
    type <- component$type
    result <- switch(type,
                     module = runModule(value, inputObjects, pipelinePath),
                     ## FIXME: running pipelines probably doesn't work
                     pipeline = runPipeline(value),
                     ## if type is incorrect
                     stop(paste0("Component '", component$name,
                                 "' has an invalid type: '",
                                 component$type, "'")))
    result
}
