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
#'
#' @return list containing \code{component} and \code{output}
start.pipe <- function(x) {
    x$start
}

#' Return \code{pipe} end list
#'
#' @param x \code{pipe} object
#' 
#' @return list containing \code{component} and \code{input}
end.pipe <- function(x) {
    x$end
}
