### Functions for loading, running and creating components

#' Run a component object
runComponent <- function(component, inputs=list(), pipelinePath) {
    value <- component$value
    type <- component$type
    ## FIXME: write case for when value not loaded in component
    result <- switch(component$type,
                     module = runModule(value, inputs, pipelinePath),
                     pipeline = runPipeline(value))
    result
}

#' Create a component object
#'
#' Create a \code{component} object for use in a \code{pipeline}.
#'
#' A \code{component} object is expected to provide either a \code{ref}, and
#' possibly a \code{path}, to an openapi XML fileIf an xml file, or a
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
    if (!is.null(ref)) {
        value <- NULL
    }
    if (!is.null(value)) {
        type <- class(value)
    }
    if (type != "module" && type != "pipeline") {
        stop("A component must be a module or a pipeline")
    }
    component <- list(name=name, ref=ref, path=path, type=type, value=value)
    class(component) <- "component"
    component
}
