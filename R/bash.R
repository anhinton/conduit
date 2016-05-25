### Platform support for bash platform

#' @describeIn prepareScriptInit
#'
#' Init script for bash script langage
prepareScriptInit.bashModuleLanguage <- function(moduleLanguage) {
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
#' bash language
#'
#' @export
internalInputScript.bashSymbol <- function (symbol) {
    paste0(symbol, "=$(cat ", symbol,
           internalExtension(moduleLanguage("bash")), ")")
}

#' @describeIn internalOutputScript create script to create internal
#' output for language = "bash"
#'
#' @export
internalOutputScript.bashSymbol <- function (symbol) {
    paste0("echo ${", symbol, "} > ", symbol, ".txt")
}

#' @describeIn command generate a command to execute a bash language
#'     script
#'
#' @export
command.bashScript <- function(script) {
    command <- list(command = "/bin/bash",
                    args = script)
    class(command) <- "command"
    command
}
