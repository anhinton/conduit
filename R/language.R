## Language tools

#' Select execLanguage to store in \code{moduleLanguage} object
#'
#' @details For most languages this function will simply return
#'     \code{language}. For \samp{python} this function returns
#'     \samp{python2} or \samp{python3} depending on the version
#'     specifications; the default is \samp{python3}.
#'
#' @param language execution language as character string
#' @param minVersion minimum language version as character string
#' @param maxVersion maximum language version as character string
#' @param version exact language version as character string
#'
#' @return execLanguage as character
#' 
#' @seealso \code{moduleLanguage}
#'
#' @import rPython
execLanguage <- function(language, minVersion = NULL, maxVersion = NULL,
                         version = NULL) {
    ## default to python3
    if (language == "python") {
        threshold = "3.0.0"
        python.exec(python.code = "from distutils.version import LooseVersion")
        python.assign(var.name = "version", value = version)
        python.assign(var.name = "maxVersion", value = maxVersion)
        python.assign(var.name = "threshold", value = threshold)
        python.exec(
            python.code = c(
                "try:",
                "    max2 = LooseVersion(maxVersion) < LooseVersion(threshold)",
                "except AttributeError:",
                "    max2 = False"))
        python.exec(
            python.code = 
                c("try:",
                  "    ver2 = LooseVersion(version) < LooseVersion(threshold)",
                  "except AttributeError:",
                  "    ver2 = False"))
        if (python.get(var.name = "max2")
            || python.get(var.name = "ver2")) {
            language = "python2"
        } else {
            language = "python3"
        }
    }
    language
}

#' Returns the correct file extension for a language's 'internal' files
#'
#' @param moduleLanguage \code{moduleLanguage} object
#' 
#' @return file exension as character as ".EXT"
internalExtension <- function(moduleLanguage) {
    if (!inherits(moduleLanguage, "moduleLanguage"))
        stop("moduleLanguage object required")
    extension <- switch(getLanguage(moduleLanguage),
                        R = ".rds",
                        python2 =, python3= ".pickle",
                        bash = ".txt",
                        stop(paste("language", language, "not supported")))
    extension
}

#' Returns the correct file extension for a language's script files
#'
#' @param moduleLanguage \code{moduleLanguage} object
#' 
#' @return file extension as character ".EXT"
scriptExtension <- function(moduleLanguage) {  
    if (!inherits(moduleLanguage, "moduleLanguage"))
        stop("moduleLanguage object required")  
    extension <- switch(getLanguage(moduleLanguage),
                        R = ".R",
                        python2 =, python3 = ".python",
                        bash = ".sh",
                        stop(paste("language", language, "not supported")))
    extension
}

