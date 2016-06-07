### Platform support for bash platform

#' @describeIn prepareScriptInit
#'
#' Init script for bash script langage
prepareScriptInit.bashModuleLanguage <- function(moduleLanguage) {
    template <- readLines(system.file("scriptTemplates", "script.sh",
                                      package = "conduit"))
    ## ## as of 2016-05-30 no version testing done for BASH
    ## data <- list(minVersion = moduleLanguage$minVersion,
    ##              maxVersion = moduleLanguage$maxVersion,
    ##              version = moduleLanguage$version)
    ## initScript <- whisker::whisker.render(template = template,
    ##                                       data = data)
    initScript <- template
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
