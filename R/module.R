### Functions for loading, saving, running, creating modules

## sourceOrder()
## arguments:
## - sources: list of sources extracted from module xml
## description:
##   for a given list of sources read in order of declaration from a module .xml
##   file, returns a numeric vector of order in which sources should be run
##   according to their "order" attributes.
##   Order goes negative < 0 < no order given < positive
sourceOrder <- function(sources) {
    ## extract order values from sources
    orderValues <- sapply(sources,
                          function(x) {
                              as.numeric(x$order)
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

## loadModule()
## arguments:
## - filename: file path of module .xml file
## - namespaces: special namespaces used in .xml file
## description:
##   loads a module from a module .xml file
##   returns a list containing:
##   - description
##   - inputs,
##   - platform,
##   - sources
##   - outputs
#' Read a module from an XML file
#' @param filename File path of module XML document
#' @param namespaces Namespaces used in XML document as named character vector
#' @return \code{module} list
#' @export
#' @import XML
#' @import RCurl
#' @import tools
#' @import RBGL
#' @import graph
loadModule <- function(name, ref, path=defaultSearchPaths,
                       namespaces=c(oa="http://www.openapi.org/2014/")) {
    xml <- fetchRef(ref, path)
    module <- xmlRoot(xmlParse(xml))
    descNodes <- getNodeSet(module, "//description|//oa:description",
                            namespaces=namespaces)
    description <-
        if (length(descNodes)) {
            xmlValue(descNodes[[1]])
        } else {
            character(1)
        }
    inputNodes <- getNodeSet(module, "//input|//oa:input",
                             namespaces=namespaces)
    inputs <- 
        if (length(inputNodes) == 0) {
            NULL
        } else {
            inputNames <- sapply(inputNodes, function(inputNodes) {
                attrs <- xmlAttrs(inputNodes)
                attrs[["name"]]
            })
            inputs <- lapply(inputNodes, function(node) {
                attrs <- xmlAttrs(node)
                name <- attrs[["name"]]
                type <- attrs[["type"]]
                formatNode <- xmlChildren(node)$format
                format <- if (length(formatNode)) {
                    formAttrs <- xmlAttrs(formatNode)
                    formatType <-
                        if (any(names(formAttrs) == "formatType")) {
                            formAttrs[["formatType"]]
                        } else {
                            "text"
                        }
                    list(format=xmlValue(formatNode), type=formatType)
                } else {
                    list(format="", type="text")
                }
                c("name"=name, "type"=type, "format"=format$format,
                  "formatType"=format$type)
            })
            names(inputs) <- inputNames
            inputs
        }
    platformNode <- getNodeSet(module, "//platform|//oa:platform",
                                   namespaces=namespaces)[[1]]
                                        # only one platform
    platform <- xmlAttrs(platformNode)[["name"]]
    names(platform) <- "name"
    sourceNodes <- getNodeSet(module, "//source|//oa:source",
                                 namespaces=namespaces)
    sources <-
        lapply(sourceNodes,
               function(node, path) {
                   attrs <- xmlAttrs(node)
                   type <-
                       if (length(attrs) && names(attrs) == "type") {
                           attrs[["type"]]
                       } else {
                           character(1)
                       }
                   order <-
                       if (length(attrs) &&
                           any(grepl("order", names(attrs)))) {
                           attrs[["order"]]
                       } else {
                           character(1)
                       }
                   value <-
                       if (length(attrs) &&
                           any(grepl("ref", names(attrs)))) {
                           ref <- attrs[["ref"]]
                           path <-
                               if (any(grepl("path", names(attrs)))) {
                                   attrs[["path"]]
                               } else {
                                   path
                               }
                           value <- fetchRef(ref, path)
                       } else {
                           xmlValue(node)
                       }
                   list("value"=value, "type"=type, "order"=order)
               }, path)
    ## arrange sources in correct order
    sources <- lapply(sourceOrder(sources),
                      function (x, sources) {
                          sources[[x]]
                      }, sources)
    outputNodes <- getNodeSet(module, "//output|//oa:output",
                                 namespaces=namespaces)
    outputs <-
        if (length(outputNodes) == 0) {
            NULL
        } else {
            outputNames <- sapply(outputNodes, function(outputNodes) {
                attrs <- xmlAttrs(outputNodes)
                attrs[["name"]]
            })
            outputs <- lapply(outputNodes, function(node) {
                attrs <- xmlAttrs(node)
                name <- attrs[["name"]]
                type <- attrs[["type"]]
                formatNode <- xmlChildren(node)$format
                format <- if (length(formatNode)) {
                    formAttrs <- xmlAttrs(formatNode)
                    formatType <-
                        if (any(names(formAttrs) == "formatType")) {
                            formAttrs[["formatType"]]
                        } else {
                            "text"
                        }
                    list(format=xmlValue(formatNode), type=formatType)
                } else {
                    list(format="", type="text")
                }
                ref <- if (type == "external") {
                    attrs[["ref"]]
                } else {
                    character(1)
                }
                c("name"=name, "type"=type, "format"=format$format,
                  "formatType"=format$type, "ref"=ref)
            })
            names(outputs) <- outputNames
            outputs
        }
    module(name=name, description=description,
           platform=platform, inputs=inputs, outputs=outputs,
           sources=sources)
}

## functions for saving a module object to an XML file

## saveModule ()
## arguments:
## - module: module object from loadModule() or module()
## - targetDirectory: where to save .xml file
## description:
##   saves a module to an .xml file on disk
#' @export
saveModule <- function(module, targetDirectory=getwd()) {
    if (!file.exists(targetDirectory)) {
        stop("no such target directory")
    }
    moduleDoc <-
        newXMLDoc(namespaces="http://www.openapi.org/2014",
                  node=newXMLNode("module", attrs=c(name=module$name),
                      namespaceDefinitions="http://www.openapi.org/2014/"))
    moduleRoot <- xmlRoot(moduleDoc)
    description <- newXMLNode("description", module$description)
    inputs <-
        lapply(module$inputs,
               function(i) {
                   input <- newXMLNode("input", attrs=c(i["name"], i["type"]))
                   input <-
                       addChildren(input,
                                   newXMLNode("format",
                                              attrs=c(i["formatType"])))
               })
    outputs <-
        lapply(module$outputs,
               function(o) {
                   attrs <- c(o["name"], o["type"])
                   if (o["type"] == "external") {
                       attrs <- c(attrs, o["ref"])
                   }
                   output <- newXMLNode("output", attrs=attrs)
                   output <-
                       addChildren(output,
                                   newXMLNode("format",
                                              attrs=c(o["formatType"])))
               })
    platform <- newXMLNode("platform", attrs=c(module$platform))
    sources <-
        lapply(module$sources,
               function (s) {
                   newXMLNode(name="source", attrs=c(s["type"], s["order"]),
                              newXMLCDataNode(s$value))
                   
               })
    moduleRoot <- addChildren(moduleRoot,
                              kids=list(description, platform, inputs, sources,
                                  outputs))
    moduleFilePath <- file.path(targetDirectory,
                                paste(module$name, ".xml", sep=""))
    saveXML(moduleDoc, moduleFilePath)
}

## RUNNING A MODULE

## runPlatform()
## arguments:
##   - module: the module to be run
##   - inputs: list of sources for inputs
##   - moduleFiles: path where module will be run
## Passes a module to the appropriate platform support
runPlatform <- function(module, inputs, moduleFiles) {
    UseMethod("runPlatform")
}

## runModule()
## arguments:
##   - module: module to be run
##   - inputs: named list of absolute paths for input files
##   - targetDirectory: where the module output is to be created
## This function:
##   - creates a directory for the module to be run in
##   - determines which platform the module requires
##   - runs the module on its platform
#' @export
runModule <- function(module, inputs=list(),
                      targetDirectory=getwd()) {
    moduleName <- module$name
    ## create a directory for this module's output
    modulePath <- file.path(targetDirectory, "modules", moduleName)
    if (file.exists(modulePath))
        unlink(modulePath, recursive=TRUE)
    dir.create(modulePath, recursive=TRUE)
    moduleFiles <- file_path_as_absolute(modulePath)

    ## set the module class to PLATFORM
    modulePlatform <- module$platform
    class(module) <- modulePlatform

    ## run this module with the appropriate Platform Support
    runPlatform(module, inputs, modulePath)
}

## module creation functions

#' @export
moduleInput <- function(name, type, format="", formatType="text") {
    c(name=name, type=type, format=format, formatType=formatType)
}
#' @export
moduleOutput <- function(name, type, format="", formatType="text", ref="") {
    c(name=name, type=type, format=format, formatType=formatType, ref=ref)
}
#' @export
moduleSource <- function(value, ref=NULL, type="", order="") {
    if (!is.null(ref)) value <- readLines(ref)
    list(value=value, type=type, order=order)
}

## module()
## arguments:
## - name: module name - character
## - description - character
## - platform - character
## - inputs - list(moduleInput(), ...)
## - outputs - list(moduleOutput(), ...)
## - sources - list(moduleSource(), ...)
## description:
##   creates a module object
##   returns a list containing:
##   - name
##   - description
##   - inputs,
##   - platform,
##   - sources
##   - outputs
#' @export
module <- function(name, description="", platform, inputs=list(),
                   outputs=list(), sources=list()) {
    names(platform) <- "name"
    if (!is.null(inputs)) {
        names(inputs) <-
            sapply(inputs,
                   function (x) {
                       x["name"]
                   })
    }
    if (!is.null(outputs)) {
        names(outputs) <-
            sapply(outputs,
                   function (x) {
                       x["name"]
                   })
    }
    module <- list(name=name, description=description,
         platform=platform, inputs=inputs, outputs=outputs,
         sources=sources)
    class(module) <- "module"
    module
}
