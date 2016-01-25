### Functions for validating modules and pipelines

#' Check module and pipeline XML for validity
#'
#' Check validity of an XML document against an XML Schema.
#'
#' Default schema is pipeline.xsd, which includes module.xsd.
#'
#' @param file XML file to be validated
#' @param type \dQuote{module} of \dQuote{pipeline}
#'
#' @return TRUE if valid
#' @import XML
isValidXML <-function (file, type) {
    schema <- system.file("xmlSchema", paste0(type, ".xsd"),
                          package = "conduit")
    value <- xmlSchemaValidate(schema, file)
    if (value$status == 0) {
        TRUE
    } else {
        FALSE
    }    
}

#' Validate a pipe
#'
#' Validates a single \code{pipe} against a \code{pipeline}'s
#' \code{component}s.
#'
#' A pipe is valid if its startComponent, startOutput, endComponent
#' and endInput can all be matched by name to elements in the
#' \code{componentList}. The function should return a warning for each
#' invalid element.
#'
#' @param pipe \code{pipe} object
#' @param componentList \code{pipeline} components
#'
#' @return Logical value, TRUE if valid
validatePipe <- function(pipe, componentList) {
    if (!inherits(pipe, "pipe"))
        stop("pipe object required")
    if (!all(sapply(componentList, inherits, what = "component")))
        stop("list of component objects required")
    
    startComponent <- startComponent(pipe)
    componentOutputs <- componentList[[startComponent]]$value$outputs
    startOutput <- startOutput(pipe)
    endComponent <- endComponent(pipe)
    componentInputs <- componentList[[endComponent]]$value$inputs
    endInput <- endInput(pipe)
    pipeName <- paste0(startComponent, ":", startOutput, " -> ",
                       endComponent, ":", endInput)
    componentNames <- names(componentList)

    ## initialise problems list
    problems <- list()

    ## check for existence of startComponent
    if (!(startComponent %in% names(componentList))) {
        problems <- c(
            problems,
            warning(paste0("Start component '", startComponent,
                           "' in pipe '", pipeName, "' does not exist")))
    }
    
    ## check for existence of startOutput
    else if (!(startOutput %in% names(componentOutputs))) {
        problems <-
            c(problems,
              warning(paste0("Start output '",
                             startOutput, "' in pipe '", pipeName,
                             "' does not exist")))
    }
    ## check for existence of endComponent
    if (!(endComponent %in% componentNames)) {
        problems <-
            c(problems,
              warning(paste0("End component '", endComponent,
                             "' in pipe '", pipeName, "' does not exist")))
    }

    ## check for existence of endInput
    else if (!(endInput %in% names(componentInputs))) {
        problems <-
            c(problems,
              warning(paste0("End input '", endInput,
                             "' in pipe '", pipeName, "' does not exist")))
    }
    
    ## valid if no problems
    if (length(problems)) FALSE else TRUE
}

#' Validate all pipes in a pipeline
#'
#' This function checks that all pipes are valid, and returns FALSE if any
#' are invalid.
#'
#' @details A pipe is considered valid if its \code{startComponent} and
#' \code{endComponent}s values match the value of name \code{component}s in
#' the pipeline, and if its \code{startOutput} and \code{endInput} values match
#' a named output or input in the corresponding \code{component}.
#'
#' A \code{warning} is generated for each invalid \code{pipe} to help debug.
#' 
#' @param pipeline A \code{pipeline} object
#'
#' @return logical, TRUE if pipeline passes all validity tests
#'
#' @seealso \code{pipeline}, \code{pipe}
#'
#' @examples
#' ## load a pipeline
#' pipel1 <-
#'     loadPipeline("simpleGraph",
#'                  system.file("extdata", "simpleGraph",
#'                              "pipeline.xml",
#'                              package = "conduit"))
#' ## a valid pipeline
#' validatePipeline(pipel1)
#'
#' ## break a pipeline
#' pipel2 <- pipel1
#' pipel2$pipes[[1]]$start$output <- "veryWrongValue"
#' ## and look, it's invalid!
#' validatePipeline(pipel2)
#' 
#' @export
validatePipeline <- function(pipeline) {
    if(!inherits(pipeline, "pipeline"))
        stop("pipeline object required")
    pipeList <- getPipes(pipeline)
    componentList <- getComponents(pipeline)
    ## check if pipes are valid
    all(sapply(pipeList, validatePipe, componentList))    
}
