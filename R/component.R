### Functions for loading, running and creating components

#' Load a component's value from XML
#'
#' Load a \code{component}'s value from the XML file given in its ref and
#' path slots.
#'
#' @param component A \code{component} object
#' @return \code{component} object with the corresponding \code{pipeline}
#' or \code{module} object its value slot
loadComponent <- function(component) {
    name <- component$name
    ref <- component$ref
    path <- component$path
    type <- component$type
    value <- switch(type,
                    module = loadModule(name, ref, path),
                    ## FIXME: I bet loading a pipeline won't work
                    pipeline = loadPipeline(ref))
    component(name=name, value=value)
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
    ## FIXME: define case when ref is given
    xml <- switch(type,
                  module = moduleToXML(value, namespaceDefinitions),
                  pipeline = pipelineToXML(value, namespaceDefinitions=NULL))
    xml
}

#' Export a component to an XML file
#'
#' @param component \code{component} object
#' @param targetDirectory File path for pipeline output
#' @param filename Name of resulting file
#' @return Resulting file path
exportComponent <- function(component, targetDirectory=getwd(),
                            filename=paste0(component$name, ".xml")) {
    if (!file.exists(targetDirectory)) {
        stop("no such target directory")
    }
    if (!is.null(component$ref)) {
        ## FIXME: this assumes a component is a module
        component$value <- loadModule(component$name, component$ref,
                                      component$path)
    }
    componentDoc <-
        newXMLDoc(namespaces="http://www.openapi.org/2014",
                  node=componentToXML(component,
                      namespaceDefinitions="http://www.openapi.org/2014/"))
    componentFilePath <- file.path(targetDirectory, filename)
    saveXML(componentDoc, componentFilePath)
}

#' Run a component object
#'
#' @param component \code{component} object
#' @param inputs Named list of absolute paths for component inputs
#' @param pipelinePath Pipeline output directory
#' @return Result of \code{runModule} or \code{runPipeline}
#' @export
runComponent <- function(component, inputs=list(), pipelinePath=getwd()) {
    if (!is.null(component$ref)) {
        if (is.null(component$path)) components$path <- defaultSearchPaths
        component <- loadComponent(component)
    }
    value <- component$value
    type <- component$type
    result <- switch(component$type,
                     module = runModule(value, inputs, pipelinePath),
                     ## FIXME: running pipelines probably doesn't work
                     pipeline = runPipeline(value))
    result
}

#' Create a component object
#'
#' Create a \code{component} object for use in a \code{pipeline}.
#'
#' A \code{component} object is expected to provide either a \code{ref}, and
#' possibly a \code{path}, to an openapi XML file, or a
#' \code{pipeline} or \code{module} object in \code{value}. Accordingly:
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
component <- function(name, value=NULL, type=NULL, ref=NULL, path=NULL) {
    ## if a 'ref' is given then we ignore given 'value' as 'value' will
    ## be read from the file given in 'ref' using loadComponent()
    if (!is.null(ref)) {
        value <- NULL
    }
    ## if a 'value' is given use this to determine the component type
    if (!is.null(value)) {
        type <- class(value)
    }
    ## if type is not 'module' or 'pipeline' then something is wrong
    if (type != "module" && type != "pipeline") {
        stop("A component must be a module or a pipeline")
    }
    component <- list(name=name, ref=ref, path=path, type=type, value=value)
    class(component) <- "component"
    component
}
