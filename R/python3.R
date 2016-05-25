### Platform support for python3 platform

#' @describeIn prepareScriptInit
#'
#' Init script for python3 langage
prepareScriptInit.python3ModuleLanguage <- function(moduleLanguage) {
    initScript <- c(
        "#!/usr/bin/python3", "",
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
        "with open('.languageVersion', 'w', encoding='UTF-8') as outFile:", 
        "    n = outFile.write(languageVersion + '\\n')",
        "")
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
