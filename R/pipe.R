#' Creates a \code{pipe} object
#'
#' Constructor method for a \code{pipe} object which connects the
#' \code{startComponent}'s \code{startOutput} to the
#' \code{endComponent}'s \code{endInput}.
#'
#' Accessor methods are defined to extract \code{startComponent},
#' \code{startOutput}, \code{endComponent} and \code{endInput}
#' elements.
#'
#' @param startComponent Name of start component
#' @param startOutput Name of start output
#' @param endComponent Name of end module
#' @param endInput Name of end input
#'
#' @return The constructor returns a \code{pipe} object connecting
#'     \code{startComponentName}.\code{startOutputName} to
#'     \code{endComponentName}.\code{endInputName}
#' 
#' @seealso \code{pipeline}, \code{addPipe}
#'
#' @examples
#' pipe1 <- pipe(startComponent = "setX", startOutput = "x",
#'               endComponent = "showY", endInput = "y")
#' 
#' @export
pipe <- function (startComponent, startOutput,
                  endComponent, endInput) {
    if (!all(sapply(
             list(startComponent, startOutput, endComponent, endInput),
             is_length1_char))) {
        stop("arguments should be length 1 character values")
    }
    start <- list(component = startComponent, output = startOutput)
    end <- list(component = endComponent, input = endInput)
    pipe <- list(start = start, end = end)
    class(pipe) <- "pipe"
    pipe
}

#' Return \code{pipe} start list
#'
#' @param x \code{pipe} object
#' @param ... additional arguments
#'
#' @return list containing \code{component} and \code{output}
#'
#' @export
start.pipe <- function(x, ...) {
    x$start
}

#' Return \code{pipe} end list
#'
#' @param x \code{pipe} object
#' @param ... additional arguments
#' 
#' @return list containing \code{component} and \code{input}
#'
#' @export
end.pipe <- function(x, ...) {
    x$end
}

#' Return \code{pipe} start component
#'
#' @param pipe \code{pipe} object
#'
#' @return name of start component as string
#'
#' @export
startComponent <- function(pipe) {
    start(pipe)$component
}

#' Return \code{pipe} start output
#'
#' @param pipe \code{pipe} object
#'
#' @return name of start output as string
#'
#' @export
startOutput <- function(pipe) {
    start(pipe)$output
}

#' Return \code{pipe} end component
#'
#' @param pipe \code{pipe} object
#'
#' @return name of end component as string
#'
#' @export
endComponent <- function(pipe) {
    end(pipe)$component
}

#' Return \code{pipe} end input
#'
#' @param pipe \code{pipe} object
#'
#' @return name of end input as string
#'
#' @export
endInput <- function(pipe) {
    end(pipe)$input
}

#' Convert a pipe to XML
#'
#' @param pipe \code{pipe} object
#' @param namespaceDefinitions XML namespaces as character vector
#'
#' @return \code{xmlNode} object
#'
#' @import XML
pipeToXML <- function(pipe, namespaceDefinitions = NULL) {
    start <- start(pipe)
    end <- end(pipe)
    startAttrs <- c(component = start$component,
                    output = start$output)
    endAttrs <- c(component =end$component,
                  input = end$input)
    pipeXML <- newXMLNode("pipe")
    pipeXML <-
        addChildren(
            node = pipeXML,
            kids=list(newXMLNode(name = "start", attrs=startAttrs),
                      newXMLNode(name = "end", attrs=endAttrs)))
    pipeXML
}
