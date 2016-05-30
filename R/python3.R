### Platform support for python3 platform

#' @describeIn prepareScriptInit
#'
#' Init script for python3 langage
prepareScriptInit.python3ModuleLanguage <- function(moduleLanguage) {
    template <- readLines(system.file("scriptTemplates", "script3.py",
                                      package = "conduit"))
    data <- list(minVersion = moduleLanguage$minVersion,
                 maxVersion = moduleLanguage$maxVersion,
                 version = moduleLanguage$version)
    initScript <- whisker::whisker.render(template = template,
                                          data = data)
    initScript
}

#' @describeIn internalInputScript prepare internal input script for
#' python3 language
#'
#' @export
internalInputScript.python3Symbol <- function (symbol) {
    c(paste0("with open('", symbol,
             internalExtension(moduleLanguage("python3")),
             "', 'rb') as f:"),
      paste0("    ", symbol, " = pickle.load(f)"))
}

#' @describeIn internalOutputScript create script to create internal
#' output for language = "python3"
#'
#' @export
internalOutputScript.python3Symbol <- function (symbol) {
    c(paste0("with open('", symbol, ".pickle', 'wb') as f:"),
      paste0("    pickle.dump(", symbol, ", f)"))
}


#' @describeIn command generate a command to execute a python3 language
#'     script
#'
#' @export
command.python3Script <- function(script) {
    command <- list(command = "python3",
                    args = script)
    class(command) <- "command"
    command
}
