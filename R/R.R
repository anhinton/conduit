### R language support

#' @describeIn prepareScriptInit
#'
#' Init script for R langage
prepareScriptInit.RModuleLanguage <- function(moduleLanguage) {
    template <- readLines(system.file("scriptTemplates", "script.R",
                                      package = "conduit"))
    data <- list(minVersion = moduleLanguage$minVersion,
                 maxVersion = moduleLanguage$maxVersion,
                 version = moduleLanguage$version)
    initScript <- whisker::whisker.render(template = template,
                                          data = data)
    initScript
}

#' @describeIn internalInputScript prepare internal input script for R
#'     language
#'
#' @export
internalInputScript.RSymbol <- function(symbol) {
    paste0(symbol, " <- readRDS(\"",
           paste0(symbol, internalExtension(moduleLanguage("R"))), "\")")
}

#' @describeIn internalOutputScript create script to create internal
#'     output for language = "R"
#'
#' @export
internalOutputScript.RSymbol <- function (symbol) {
    paste0("saveRDS(", symbol, ", file = \"", symbol, ".rds\")")
}

#' @describeIn command generate a command to execute an R language
#'     script
#'
#' @export
command.RScript <- function(script) {
    command <- list(command = "Rscript",
                    args = script)
    class(command) <- "command"
    command
}
