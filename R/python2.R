### Platform support for python2 platform

#' @describeIn prepareScriptInit
#'
#' Init script for python2 langage
prepareScriptInit.python2ModuleLanguage <- function(moduleLanguage) {
    template <- readLines(system.file("scriptTemplates", "script2.py",
                                      package = "conduit"))
    data <- list(minVersion = moduleLanguage$minVersion,
                 maxVersion = moduleLanguage$maxVersion,
                 version = moduleLanguage$version)
    initScript <- whisker::whisker.render(template = template,
                                          data = data)
    initScript
}

#' @describeIn internalInputScript prepare internal input script for
#' python2 language
#'
#' @export
internalInputScript.python2Symbol <- function (symbol) {
    c(paste0("with open('", symbol,
             internalExtension(moduleLanguage("python2")),
             "', 'rb') as f:"),
      paste0("    ", symbol, " = pickle.load(f)"))
}

#' @describeIn internalOutputScript create script to create internal
#' output for language = "python2"
#'
#' @export
internalOutputScript.python2Symbol <- function (symbol) {
    c(paste0("with open('", symbol, ".pickle', 'wb') as f:"),
      paste0("    pickle.dump(", symbol, ", f)"))
}


#' @describeIn command generate a command to execute a python2 language
#'     script
#'
#' @export
command.python2Script <- function(script) {
    command <- list(command = "python2",
                    args = script)
    class(command) <- "command"
    command
}
