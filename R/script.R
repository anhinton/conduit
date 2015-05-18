#' prepare internalVessel input script for python language
internalVesselScript.python <- function (symbol, resource) {
    
}

#' prepare internalVessel input script for R language
internalVesselScript.R <- function (symbol, resource) {
    script <- paste0(symbol, " <- readRDS(\"", resource, "\")")
    return(script)
}

#' prepare internalVessel input script
#'
#' @param symbol character string with class set to language of module script
#' @param resource file location of serialised language object
#'
#' @return character vector of script to ensure input
internalVesselScript <- function (symbol, resource) {
    UseMethod("internalVesselScript")
}

#' ensure input described by internalVessel is satisfied
#'
#' @param symbol language symbol to be satisfied
#' @param resource location of serialised language object
#' @param language module script language
#'
#' @return script as character vector
#'
#' @seealso \code{moduleInputScript}, \code{executeScript}
ensureInternalVessel <- function (symbol, resource, language) {
    class(symbol) <- language
    script <- internalVesselScript(symbol, resource)
    return(script)
}

#' ensure input described by fileVessel is satisfied
#'
#' @param ref file path to be satisfied
#' @param resource file path to be passed to ref
#'
#' @return NULL
ensureFileVessel <- function (ref, resource) {
    ## check if ref already exists
    if (file.exists(ref)) {
        ## check if symlink
        if (Sys.readlink(ref) == "") {
            stop("input file already exists")
        }
        ## if ref is a symlink assume created by conduit and remove
        file.remove(ref)
    }

    ##  create a symlink to resource at ref
    file.symlink(resource, ref)
    return(NULL)
}

#' ensure module inputs will be satisfied
#'
#' @param input \code{moduleInput} object
#' @param resource address of resource to be supplied as input
#' @param language language of module script
#'
#' @return character string containing script to ensure input or NULL
ensureModuleInput <- function (input, resource, language) {
    vessel <- input$vessel
    type <- switch(
        class(vessel)[[1]],
        internalVessel = "internal",
        fileVessel = "file"
    )
    script <- switch(
        type,
        internal = ensureInternalVessel(vessel$symbol, resource, language),
        file = ensureFileVessel(vessel$ref, resource)
    )
    
    return(script)
}

#' create script to create internal output for language = "R"
internalOutputScript.R <- function (symbol) {
    script <- paste0("saveRDS(", symbol, ", file = \"", symbol, ".rds\")")
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
