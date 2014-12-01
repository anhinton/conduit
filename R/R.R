### Platform support for R platform

#' Platform support for "R" platform.
#'
#' @details Creates a .R script file from the supplied \code{module},
#' taking specific input file paths from \code{inputs}.
#'
#' Writes script to \code{modulePath}, then attempts to execute the
#' script in this location.
#'
#' @param module \code{module} object
#' @param inputs Named list of input locations
#' @param modulePath File path for module output
#' @return FIXME: nothing meaningful
runPlatform.R <- function(module, inputs, modulePath) {
    ## sourceScript contains the module's source(s) to be evaluated
    sourceScript <-
        lapply(module$sources,
               function (x) {
                   x["value"]
               })
    sourceScript <- unlist(sourceScript)
    ## directoryScript ensures the platform call is run in the module's
    ## assigned directory
    directoryScript <-
        paste0("setwd(\"", modulePath, "\")")
    ## inputScript loads the module's designated inputs
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
                            paste0(inputName, " <- readRDS(\"", fromFile,
                                   "\")")
                        } else if (type == "external") {
                            paste0(inputName, " <- \"", fromFile, "\"")
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
                         paste0("saveRDS(", name, ", file=\"", name, ".rds\")")
                    } else {
                        character(1)
                    }
                })
        }
    ## moduleScript combines the scripts in correct order
    moduleScript <- c(directoryScript, inputScript, sourceScript, outputScript)
    
    ## write script file to directory
    scriptPath <- file.path(modulePath, "script.R")
    scriptFile <- file(scriptPath)
    writeLines(moduleScript, scriptFile)
    close(scriptFile)

    ## run the script in an R session
    ## FIXME: I have added quotes around the 'scriptPath' when constructin
    ## systemCall to handle a case where the file path contains spaces.
    ## This needs to be applied to ALL platform supports. Possibly this part
    ## of platform support needs to be generalised as a function.
    systemCall <- paste0("Rscript \"", scriptPath, "\"")
    try(system(systemCall))
}
