### Platform support for shell platform

#' Platform support for "shell" platform.
#'
#' @details Creates a .sh script file from the supplied \code{module},
#' taking specific input file paths from \code{inputs}.
#'
#' Writes script to \code{modulePath}, then attempts to execute the
#' script in this location.
#'
#' @param module \code{module} object
#' @param inputs Named list of input locations
#' @param modulePath File path for module output
#' @return FIXME: nothing meaningful
runPlatform.shell <- function(module, inputs, modulePath) {
    ## sourceScript contains the module's source(s) to be evaluated
    sourceScript <-
        lapply(module$sources,
               function (s) {
                   s["value"]
               })
    sourceScript <- unlist(sourceScript)
    ## directoryScript ensures the platform call is run in the module's
    ## assigned directory
    directoryScript <-
        paste0("cd ", modulePath)
    inputScript <- outputScript <- character(1)
    inputScript <-
        if (length(module$inputs)) {
            sapply(
                module$inputs,
                function (x) {
                    inputName <- x["name"]
                    type <- x["type"]
                    fromFile <- inputs[[inputName]]
                    input <-
                        if (type == "internal") {
                            value <- readLines(fromFile, n=1)
                            paste0(inputName, "=", value)
                        } else if (type == "external") {
                            paste0(inputName, "=", fromFile)
                        }
                })
        }
    ## outputScript loads the module's designated outputs
    outputScript <-
        if (length(module$outputs)) {
            sapply(
                module$outputs,
                function (x) {
                    type <- x["type"]
                    if (type == "internal") {
                        name <- x["name"]
                        paste0("echo ${", name, "} > ", name, ".txt")
                    } else {
                        character(1)
                    }
                })
        }
    ## moduleScript combines the scripts in correct order
    moduleScript <- c(directoryScript, inputScript, sourceScript, outputScript)
    
    ## write script file to directory
    scriptPath <- file.path(modulePath, "script.sh")
    scriptFile <- file(scriptPath)
    writeLines(moduleScript, scriptFile)
    close(scriptFile)

    ## run the script in a PLATFORM session
    systemCall <- paste0("sh ", scriptPath)
    try(system(systemCall))
}
