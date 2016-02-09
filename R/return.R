resultInput <- function(output, modulePath) {
    name <- getName(output)
    vessel <- getVessel(output)
    type <- getType(vessel)
    format <- getFormat(output)
    switch(type,
           fileVessel =, internalVessel = {
               result <- getResult(output)
               resultref <-
                   if (dirname(result) == modulePath) {
                       basename(result)
                   } else {
                       gsub(modulePath, ".", result)
                   }        
               moduleInput(
                   name = name,
                   vessel = fileVessel(ref = resultref),
                   format = format)
           })
}

resultSource <- function(output, language, modulePath) {
    result <- getResult(output)
    resultref <-
        if (dirname(result) == modulePath) {
            basename(result)
        } else {
            gsub(modulePath, ".", result)
        }
    input <- moduleInput(name = getName(output), vessel = getVessel(output),
                         format = getFormat(output))
    script <- prepareScriptInput(input, inputObject = resultref,
                                 language = language)
    if (!is.null(script)) {
        return(moduleSource(scriptVessel(script)))
    } else {
        NULL
    }
}

returnOutput <- function(output) {
    moduleOutput(name = getName(output), vessel = getVessel(output),
                 format = getFormat(output))
}
