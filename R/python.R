### Platform support for python platform

#' @describeIn internalInputScript prepare internal input script for
#' python language
#'
#' @export
internalInputScript.pythonSymbol <- function (symbol) {
    c(paste0("with open('", symbol, internalExtension("python"),
             "', 'rb') as f:"),
      paste0("\t", symbol, " = pickle.load(f)"))
}

#' @describeIn internalOutputScript create script to create internal
#' output for language = "python"
#'
#' @export
internalOutputScript.pythonSymbol <- function (symbol) {
    c(paste0("with open('", symbol, ".pickle', 'wb') as f:"),
      paste0("\tpickle.dump(", symbol, ", f)"))
}


#' @describeIn command generate a command to execute a python language
#'     script
#'
#' @export
command.pythonScript <- function(script) {
    command <- list(command = "python",
                    args = script)
    class(command) <- "command"
    command
}
