### Platform support for R platform

## runPlatform.R
## arguments:
## - module: list object
## - inputs: named list of file locations
## - modulePath: file path in which to save outputs
## description:
##   platform support for "shell" platform.
##   Creates an R ".R" script from a given module, taking specific input file
##   paths from 'inputs'.
##   Writes script to 'filePath', then attempts to run the script in this
##   location.
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
    systemCall <- paste0("Rscript ", scriptPath)
    try(system(systemCall))
}
