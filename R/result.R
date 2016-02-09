moduleResult <- function(objects, modulePath, module) {
    name <- getName(module)
    language <- getLanguage(module)
    description <- getDescription(module)
    
    ## create result module
    inputList <- lapply(objects, resultInput, modulePath = modulePath)
    inputList <- inputList[!sapply(inputList, is.null)]
    sourceList <- lapply(objects, resultSource, language = language,
                         modulePath = modulePath)
    sourceList <- sourceList[!sapply(sourceList, is.null)]
    outputList <- lapply(objects, returnOutput)
    resultModule <- module(
        name = name,
        language = language,
        description = description,
        inputs = inputList,
        sources = sourceList,
        outputs = outputList)
    moduleFile <- saveModule(resultModule, targetDirectory = modulePath)

    ## return result module and outputs
    moduleResult <- list(file = moduleFile, module = resultModule,
                         objects = objects)
    class(moduleResult) <- "moduleResult"
    moduleResult
}

pipelineResult <- function() {
    
}

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
