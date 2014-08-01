### Platform support for shell platform

## runPlatform.shell()
## arguments:
## - module: list object
## - inputs: named list of input object file paths
## - modulePath: where this module will save files
## description:
##   platform support for "shell" platform.
##   Creates a shell script from a given module, taking specific input file
##   paths from 'inputs'.
##   Writes script to 'filePath', then attempts to run the script in this
##   location.
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
                    fromFile <- 
                        getElement(inputs,
                                   paste(module$name,inputName,sep="."))
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
