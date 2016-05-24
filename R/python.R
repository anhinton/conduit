### Platform support for python platform

#' @describeIn prepareScriptInit
#'
#' Init script for python langage
prepareScriptInit.pythonModuleLanguage <- function(moduleLanguage) {
    initScript <- c(
        "#!/usr/bin/python", "",
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
        "if (thisVersion >= LooseVersion('3.0.0')):",
        "    with open('.languageVersion', 'w', encoding='UTF-8') as outFile:", 
        "        n = outFile.write(languageVersion + '\\n')",
        "else:", 
        "    with open('.languageVersion', 'w') as outFile:",
        "        n = outFile.write(languageVersion + '\\n')", 
        "")
    initScript
}

#' @describeIn internalInputScript prepare internal input script for
#' python language
#'
#' @export
internalInputScript.pythonSymbol <- function (symbol) {
    c(paste0("with open('", symbol,
             internalExtension(moduleLanguage("python")),
             "', 'rb') as f:"),
      paste0("    ", symbol, " = pickle.load(f)"))
}

#' @describeIn internalOutputScript create script to create internal
#' output for language = "python"
#'
#' @export
internalOutputScript.pythonSymbol <- function (symbol) {
    c(paste0("with open('", symbol, ".pickle', 'wb') as f:"),
      paste0("    pickle.dump(", symbol, ", f)"))
}


#' @describeIn command generate a command to execute a python language
#'     script
#'
#' @export
command.pythonScript <- function(script) {
    command <- list(command = "python2",
                    args = script)
    class(command) <- "command"
    command
}
