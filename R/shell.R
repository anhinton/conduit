### Platform support for shell platform

#' @describeIn internalInputScript prepare internal input script for
#' shell language
internalInputScript.shellSymbol <- function (symbol) {
    paste0(symbol, "=$(cat ", symbol, internalExtension("shell"),
           ")")
}

#' @describeIn internalOutputScript create script to create internal
#' output for language = "shell"
internalOutputScript.shellSymbol <- function (symbol) {
    paste0("echo ${", symbol, "} > ", symbol, ".txt")
}

#' @describeIn command generate a command to execute a shell language
#'     script
command.shellScript <- function(script) {
    command <- list(command = "sh",
                    args = script)
    class(command) <- "command"
    command
}
