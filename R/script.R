##' prepare internal input script
#'
#' @param symbol character string with class set to language of module script
#' @param inputObject file location of serialised language object
#'
#' @return character vector of script to ensure input
internalInputScript <- function (symbol, inputObject) {
    UseMethod("internalInputScript")
}

#' ensure input described by internalVessel is satisfied
#'
#' @param symbol language symbol to be satisfied
#' @param outputObject location of serialised language object
#' @param language module script language
#'
#' @return script as character vector
#'
#' @seealso \code{moduleInputScript}, \code{executeScript}
ensureInternalInput <- function (symbol, outputObject, language) {
    class(symbol) <- language
    script <- internalInputScript(symbol, outputObject)
    return(script)
}

#' ensure input described by fileVessel is satisfied
#'
#' @param ref file path to be satisfied
#' @param outputObject file path to be passed to ref
#'
#' @return NULL
ensureFileInput <- function (ref, outputObject) {
    ##  create a copy of resource at ref
    file.copy(outputObject, ref, overwrite = TRUE)
    return(NULL)
}

#' ensure module inputs will be satisfied
#'
#' @param input \code{moduleInput} object
#' @param inputObject address of resource to be supplied as input
#' @param language language of module script
#'
#' @return character string containing script to ensure input or NULL
ensureModuleInput <- function (input, inputObject, language) {
    vessel <- input$vessel
    type <- switch(
        class(vessel)[[1]],
        internalVessel = "internal",
        fileVessel = "file"
    )
    script <- switch(
        type,
        internal = ensureInternalInput(vessel$symbol, inputObject, language),
        file = ensureFileInput(vessel$ref, inputObject)
    )
    
    return(script)
}

#' create script to create internal output
internalOutputScript <- function (symbol) {
    UseMethod("internalOutputScript")
}

#' ensure output described by internalVessel is created
#'
#' @param symbol language symbol named as output
#' @param language module script language
#'
#' @return character vector of script to create serialized output object
#'
#' @seealso \code{ensureModuleOutput} and \code{executeScript}
ensureInternalOutput <- function(symbol, language) {
    class(symbol) <- language
    script <- internalOutputScript(symbol)
    return(script)
}

#' ensure module outputs are produced
#'
#' @param output \code{moduleOutput} object
#' @param language language of module script
#'
#' @return character string containing script to produce output or NULL
#'
#' @seealso \code{executeScript}
ensureModuleOutput <- function (output, language) {
    vessel <- output$vessel
    type <- switch(
        class(vessel)[[1]],
        internalVessel = "internal",
        "undefined")
    script <- switch(
        type,
        internal = ensureInternalOutput(vessel$symbol, language),
        undefined = NULL
        )
    return(script)
}

#' Extract a module's source script from a scriptVessel
extractModuleSource.scriptVessel <- function(moduleSource) {
    script <- moduleSource$vessel$value
    return(script)
}

#' Extract a module's source script from a fileVessel
extractModuleSource.fileVessel <- function(moduleSource) {
    script <- readLines(moduleSource$vessel$ref)
    return(script)
}

#' Extract a module's source script
#'
#' @param vessel Vessel indicating source script location
#'
#' @return character vector of source scrip
#'
#' @seealso \code{executeScript}
extractModuleSource <- function(moduleSource) {
    UseMethod("extractModuleSource")
}

#' Checks a module output object has been created.
#'
#' @details Will produce an error if the object does not exist.
#'
#' @param output \code{moduleOutput} object
#' @param internalExtension file extension for serialized internal language
#' object
#'
#' @return named list containing:
#' \itemize{
#'   \item name: object name
#'   \item type: object vessel type
#'   \item object: output object
#' }
checkOutputObject <- function (output, internalExtension) {
    name <- output$name
    vessel <- output$vessel
    type <- class(vessel)[[1]]
    object <- switch(type,
                     internalVessel =
                         paste0(vessel$symbol, internalExtension),
                     fileVessel = vessel$ref,
                     stop("vessel type not defined"))
    object <- try(normalizePath(object))

    if (type == "internalVessel" || type == "fileVessel") {
        if (!file.exists(object)) {
            stop(paste0("output object '", name, "' does not exist"))
        }
    }
    object <- list(name = name, type = type, object = object)
    return(object)
}

#' Determines running order for \code{moduleSource}s.
#'
#' @details Order goes negative < 0 < no order given < positive.
#'
#' @param sources List of \code{moduleSource}s
#' 
#' @return Running order as numeric vector
#' 
#' @seealso \code{moduleSource}
sourceOrder <- function(sources) {
    ## extract order values from sources
    orderValues <- sapply(sources,
                          function(x) {
                              value <-
                                  if (is.null(x$order)) {
                                      NA
                                  } else {                              
                                      as.numeric(x$order)
                                  }
                              return(value)
                          })
    ## logical vector of which order values <= 0
    zeroLess <- !is.na(orderValues) & orderValues <= 0
    ## numeric ordering of above
    zeroLessOrder <- order(orderValues[zeroLess])
    ## indices of order values <=0 ordered by zeroLessOrder
    zeroLessOrdered <- which(zeroLess)[zeroLessOrder]
    ## pos: values > 0 ordered
    pos <- !is.na(orderValues) & orderValues > 0
    posOrder <- order(orderValues[pos])
    ## indices of order values > 0 ordered by posOrder
    posOrdered <- which(pos)[posOrder]
    ## indices of missing order values
    unorderedOrdered <- which(is.na(orderValues))
    ## negative < 0 < unordered < positive
    c(zeroLessOrdered, unorderedOrdered, posOrdered)
}

#' Execute a module source scripts.
#'
#' Execute module source scripts in the language given by
#' \code{module$language}. 
#'
#' @details A module's inputs and outputs objects are calculated, and a
#' script is placed in the working directory. The function then attemps to
#' execute this script using the specified language.
#'
#' @param module \code{module} object
#' @param inputObjects Named list of input objects
#' 
#' @return FIXME: nothing meaningful
executeScript <- function(module, inputObjects) {
    UseMethod("executeScript")
}
