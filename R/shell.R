### Platform support for shell platform

#' @describeIn prepareScriptInit
#'
#' Init script for bash script langage
prepareScriptInit.shellModuleLanguage <- function(moduleLanguage) {
    initScript <- c(
        "#!/bin/bash",
        "## CONDUIT: checking language version",
        "## for bash no actual checking is done, but the current version is returned", 
        "outfile=\".languageVersion\"",
        "echo \"$BASH_VERSION\" > $outfile", 
        "counter=0", "while [ $counter -lt 3 ];",
        "do",
        "    echo \"0\" >> $outfile", 
        "    let counter=counter+1", "done", "")
    initScript
}

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
    command <- list(command = "/bin/bash",
                    args = script)
    class(command) <- "command"
    command
}
