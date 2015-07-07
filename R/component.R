### Functions for exporting, running and creating components

#' return the name of a component
#'
#' Returns the name of a \code{module} or \code{pipeline}
#'
#' @param component \code{module} or \code{pipeline} object
#' @return character value
componentName <- function (component) {
    component$name
}

#' Convert a component to XML
#'
#' Convert a \code{component} object into the corresponding openapi XML
#' format.
#'
#' The \code{component} object \emph{must} have a \code{pipeline} or
#' \code{module} objects in its \code{value} slot.
#'
#' @param component \code{component} object
#' @param namespaceDefinitions As named character vector
#' @return \code{xmlNode} object
componentToXML <- function(component, namespaceDefinitions=NULL) {
    type <- component$type
    value <- component$value
    if (class(value) != type) {
        stop("You have provided a mismatched component object")
    }
    ## FIXME: define case when 'ref' is given. THIS FUNCTION ASSUMES
    ## that the component being passed in has a 'value', which is
    ## something all components will one day have. However, if a 'ref' is
    ## provided I want to be dealing with that
    xml <- switch(type,
                  module = moduleToXML(value, namespaceDefinitions),
                  pipeline = pipelineToXML(value, namespaceDefinitions=NULL))
    xml
}

#' Export a component to an XML file
#'
#' @param component \code{component} object
#' @param targetDirectory File path for pipeline output
#' @return Resulting file path
exportComponent <- function(component, targetDirectory=getwd()) {
    ## stop of targetDirectory doesn't exist
    if (!file.exists(targetDirectory)) {
        stop("no such target directory")
    }

    ## create XML 
    componentDoc <-
        newXMLDoc(namespaces="http://www.openapi.org/2014",
                  node=componentToXML(component,
                      namespaceDefinitions="http://www.openapi.org/2014/"))

    filename <- if (is.null(component$ref)) {
        paste0(component$name, ".xml")
    } else {
        component$ref
    }
    
    ## save XML to file
    componentFilePath <- file.path(targetDirectory, filename)
    saveXML(componentDoc, componentFilePath)
}

#' return output object produced by a module output
#'
#' @param output \code{moduleOutput} object
#' @param outputDirectory file location for module execution
#'
#' @return output object
outputObject <- function(output, language, outputDirectory) {
    vessel <- output$vessel
    type <- class(vessel)[[1]]
    outputObject <-
        switch(type,
               internalVessel =
                   paste0(vessel$symbol, internalExtension(language)),
               fileVessel = vessel$ref,
               stop("vessel type not defined"))
    if (dirname(outputObject) == ".") {
        outputObject <- file.path(outputDirectory, outputObject)
    }
    if (file.exists(outputObject)) {
        outputObject <- try(normalizePath(outputObject))
    }
    return(outputObject)
}

#' calculate output objects produced by a module
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

#' Create a component object
#'
#' Create a \code{component} object for use in a \code{pipeline}.
#'
#' @details This function requires either a \code{ref}, and possibly a
#' \code{path}, to an openapi XML file, or a \code{pipeline} or \code{module}
#' object in \code{value}. Accordingly:
#'
#' \itemize{
#' \item{if \code{ref} is given the resulting component will have
#' \code{value} coerced to NULL.}
#'
#' \item{if no \code{ref} is given the resulting component will have \code{type}
#' coerced to the class of the \code{value} object.}
#' }
#'
#' If \code{type} is not \dQuote{pipeline} or \dQuote{module} the function
#' will fail.
#'
#' @param name Name of component
#' @param value \code{pipeline} or \code{module} object
#' @param type Character value; \dQuote{pipeline} or \dQuote{module}
#' @param ref xml filename
#' @param path location of xml file
#' @return \code{component} list containing:
#' \item{name}{component name}
#' \item{ref}{xml filename}
#' \item{path}{path to xml file}
#' \item{type}{component type}
#' \item{value}{\code{pipeline} or \code{module} object}
#' @seealso \code{pipeline}, \code{module}
component <- function(name, value=NULL, type=NULL, ref=NULL, path=NULL) {
    valueClass <- class(value)

    ## fail if no ref of value provided
    if (is.null(ref) && is.null(value)) {
        stop("A component must have a ref or a value")
    }
    
    ## if no type specified, set to class of value
    if (is.null(type)) {
        type <- valueClass
    }
    
    ## fail if conflicting types given
    if (valueClass != type) {
        stop(paste0("Type mismatch! value has type: '", class(value),
                    "', but `type` is set to: '", type, "'"))
    }
    
    ## if type is not 'module' or 'pipeline' then something is wrong
    if (type != "module" && type != "pipeline") {
        stop("A component must be a module or a pipeline")
    }
    
    component <- list(name=name, ref=ref, path=path, type=type, value=value)
    class(component) <- "component"
    component
}
