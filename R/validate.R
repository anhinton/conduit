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
#' @return Logical value, TRUE if valid
#' @param pipeline A \code{pipeline} object
#' @export
validatePipeline <- function(pipeline) {
    pipes <- pipeline$pipes
    components <- pipeline$components
    ## FIXME: this duplicates work done in runPipeline(), and in both cases
    ## this SHOULD already have been done for any pipeline loaded in conduit
    ## check the startComponent exists
    components <-
        lapply(
            components,
            function (c) {
                if (!is.null(c$ref)) {
                    if (is.null(c$path)) c$path <- pipeline$path
                    ## FIXME: only handles modules, not pipelines
                    c <- loadComponent(c)
                }
                c
            })
    ## check if pipes are valid
    pipesValidity <- sapply(pipes, validatePipe, components)
    allValid <- all(pipesValidity)
    allValid
}
