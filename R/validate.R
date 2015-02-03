### Functions for validating modules and pipelines

#' Validate a pipe
#'
#' @return Logical value, TRUE if valid
#' @param p \code{pipe} object
#' @param components \code{pipeline} components
validatePipe <- function(p, components) {
    startComponent <- p$start$component
    componentOutputs <- components[[startComponent]]$value$outputs
    startOutput <- p$start$output
    endComponent <- p$end$component
    componentInputs <- components[[endComponent]]$value$inputs
    endInput <- p$end$input
    pipeName <- paste0(startComponent, ":", startOutput, " -> ",
                       endComponent, ":", endInput)
    problems <- list()
    if (!any(names(components) == startComponent)) {
        problems <-
            c(problems,
              warning(paste0("Start component '", startComponent,
                             "' in pipe '", pipeName, "' does not exist")))
    }
    ## check start output exists in start component
    else if (!any(names(componentOutputs) == startOutput)) {
        problems <-
            c(problems,
              warning(paste0("Start output '",
                             startOutput, "' in pipe '", pipeName,
                             "' does not exist")))
    }
    ## check end component exists
    if (!any(names(components) == endComponent)) {
        problems <-
            c(problems,
              warning(paste0("End component '", endComponent,
                             "' in pipe '", pipeName, "' does not exist")))
    }

    ## check end input exists in end component
    else if (!any(names(componentInputs) == endInput)) {
        problems <-
            c(problems,
              warning(paste0("End input '", endInput,
                             "' in pipe '", pipeName, "' does not exist")))
    }
    valid <- if (length(problems)) FALSE else TRUE
    valid
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
#' @return Logical value, TRUE if valid
#' @param pipeline A \code{pipeline} object
#' @return logical, TRUE if pipeline passes all validity tests
#' @seealso \code{pipeline}, \code{pipe}
#'
#' @examples
#' ## load a pipeline
#' pipel1 <-
#'     loadPipeline("simpleGraph",
#'                  system.file("extdata", "simpleGraph",
#'                              "simpleGraph-pipeline.xml",
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
    pipes <- pipeline$pipes
    components <- pipeline$components
    ## check if pipes are valid
    pipesValidity <- sapply(pipes, validatePipe, components)
    allValid <- all(pipesValidity)
    allValid
}
