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

command.shellScript <- function(script) {
    list(command = "sh",
         args = script)
}
