### Platform support for shell platform

#' @describeIn internalInputScript prepare internal input script for
#' shell language
#'
#' @export
internalInputScript.shellSymbol <- function (symbol) {
    paste0(symbol, "=$(cat ", symbol,
           internalExtension(moduleLanguage("shell")), ")")
}

#' @describeIn internalOutputScript create script to create internal
#' output for language = "shell"
#'
#' @export
internalOutputScript.shellSymbol <- function (symbol) {
    paste0("echo ${", symbol, "} > ", symbol, ".txt")
}

#' @describeIn command generate a command to execute a shell language
#'     script
#'
#' @export
command.shellScript <- function(script) {
    command <- list(command = "sh",
                    args = script)
    class(command) <- "command"
    command
}
