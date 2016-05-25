### Platform support for python2 platform

#' @describeIn prepareScriptInit
#'
#' Init script for python2 langage
prepareScriptInit.python2ModuleLanguage <- function(moduleLanguage) {
    initScript <- c(
        "#!/usr/bin/python2", "",
        "## CONDUIT: import modules",
        "import os", 
        "import pickle",
        "from platform import python_version",
        "from distutils.version import LooseVersion", 
        "",
        "## CONDUIT: checking language version",
        paste0("version = {'minVersion':LooseVersion('",
               moduleLanguage$minVersion, "'),"), 
        paste0("           'maxVersion':LooseVersion('",
               moduleLanguage$maxVersion, "'),"),
        paste0("           'version':LooseVersion('",
               moduleLanguage$version, "')}"), 
        "thisVersion = LooseVersion(python_version())",
        "try:",
        "    failMin = thisVersion < version['minVersion']", 
        "except AttributeError:",
        "    failMin = False", "try:",
        "    failMax = thisVersion > version['maxVersion']", 
        "except AttributeError:",
        "    failMax = False",
        "try:",
        "    failExact = thisVersion != version['version']", 
        "except AttributeError:",
        "    failExact = False",
        "languageVersion = [str(thisVersion), str(int(failMin)), str(int(failMax)),", 
        "                   str(int(failExact))]",
        "languageVersion = '\\n'.join(languageVersion)",
        "with open('.languageVersion', 'w') as outFile:",
        "    n = outFile.write(languageVersion + '\\n')", 
        "")
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
