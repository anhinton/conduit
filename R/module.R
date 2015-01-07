### Functions for loading, saving, running, creating modules

#' Determines running order for \code{moduleSource}s.
#'
#' @details Order goes negative < 0 < no order given < positive.
#'
#' @param sources List of \code{moduleSource}s
#' @return Running order as numeric vector
#' @seealso \code{moduleSource}
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

#' Parse module XML and return a module object
#'
#' @param name module name
#' @param xml module \code{XMLNode}
#' @param path search path (optional)
#' @return \code{module} object
#' @import XML
readModuleXML <- function(name, xml, path = NULL) {
    nodes <- xmlChildren(xml)
    ## extract description
    descNode <- nodes$description
    description <- xmlValue(descNode)
    ## extract inputs
    inputNodes <- nodes[names(nodes) == "input"]
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
    ## extract platform
    platformNode <- nodes$platform
    platform <- xmlAttrs(platformNode)[["name"]]
    ## extract sources
    sourceNodes <- nodes[names(nodes) == "source"]
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
                           ## FIXME: not well tested or even understood
                           tryCatch(
                               fetchRef(ref, path),
                               error = function (err) {
                                   stop("Unable to load module source\n",
                                        err)
                               })
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
    ## extract outputs
    outputNodes <- nodes[names(nodes) == "output"]
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
    module(name=name, path=path, description=description,
           platform=platform, inputs=inputs, outputs=outputs,
           sources=sources)
}

#' Load a module from an XML file
#'
#' Reads an XML file given by \code{ref} and \code{path} and interprets to
#' produce a \code{module}.
#'
#' If \code{path} is not set and conduit needs to search for the file the
#' default search paths are used.
#' 
#' @param name Name of module
#' @param ref Module location or filename
#' @param path (Optional) Search path if \code{ref} is a filename
#' @param namespaces Namespaces used in XML document
#' @return \code{module} list
#' @seealso \code{module}
#' @export
#' 
#' @import XML
#'
#' @examples
#'
#' ## load a module from XML given by absolute of relative file path
#' mod1xml <- system.file("extdata", "simpleGraph", "createGraph.xml",
#'                        package = "conduit")
#' mod1 <- loadModule(name = "createGraph", ref = mod1xml)
#'
#' ## load a module by searching for 'ref'
#' srch1 <- system.file("extdata", package = "conduit")
#' srch1
#'
#' mod2 <- loadModule(name = "layoutGraph", ref = "layoutGraph.xml",
#'                    path = srch1)
loadModule <- function(name, ref, path = NULL,
                       namespaces=c(oa="http://www.openapi.org/2014/")) {
    ## if path is not set, make path from ref
    if (is.null(path)) {
        path <- paste0(dirname(ref), pathSep)
        ref <- basename(ref)
    }
    ## fetch module XML from disk
    rawXML <-
        tryCatch(
            fetchRef(ref, path),
            error = function(err) {
                problem <- c(paste0("Unable to load module '", name, "'\n"),
                             err)
                stop(problem)
            })
    xml <- xmlRoot(xmlParse(rawXML))
    module <- readModuleXML(name, xml, path)
    module
}

## functions for saving a module object to an XML file

#' Convert a module to XML
#'
#' @param module \code{module} object
#' @param namespaceDefinitions XML namespaces as character vector
#' @return \code{XMLNode} object
#' @import XML
moduleToXML <- function (module,
                         namespaceDefinitions=NULL) {
    moduleRoot <- newXMLNode("module",
                             namespaceDefinitions=namespaceDefinitions)
    description <- newXMLNode("description", module$description)
    inputs <-
        lapply(
            module$inputs,
            function(i) {
                input <- newXMLNode("input", attrs=c(i["name"], i["type"]))
                format <- newXMLNode("format", attrs = c(i["formatType"]))
                xmlValue(format) <- i["format"]
                input <- addChildren(input, format)
            })
    outputs <-
        lapply(module$outputs,
               function(o) {
                   attrs <- c(o["name"], o["type"])
                   if (o["type"] == "external") {
                       attrs <- c(attrs, o["ref"])
                   }
                   output <- newXMLNode("output", attrs=attrs)
                   format <- newXMLNode("format", attrs = c(o["formatType"]))
                   xmlValue(format) <- o["format"]
                   output <- addChildren(output, format)
               })
    platform <- newXMLNode("platform", attrs=c(module$platform))
    sources <-
        lapply(module$sources,
               function (s) {
                   ## create new source node
                   sourceNode <- newXMLNode(name = "source")
                   ## if no ref is given, save 'value' inline as cdata
                   if (is.null(s$ref)) {
                       sourceNode <-
                           addChildren(
                               node = sourceNode,
                               ## collapse value script with newline
                               ## (assumes value is character vector)
                               kids = paste0(s$value, collapse = "\n"),
                               cdata = TRUE)
                   } else {
                   ## else record ref and path as attrs
                       xmlAttrs(sourceNode) <- c(s["ref"], s["path"])
                   }
                   ## set source 'type' if provided
                   if (nchar(s["type"])) {
                       xmlAttrs(sourceNode) <- c(s["type"])
                   }
                   ## set source 'order' if provided
                   if (nchar(s["order"])) {
                       xmlAttrs(sourceNode) <- c(s["order"])
                   }
                   sourceNode
               })
    moduleRoot <-
        addChildren(moduleRoot,
                    kids=list(description, platform, inputs, sources,
                        outputs))
    moduleRoot
}

#' Save a module to disk
#' 
#' Save a \code{module} to an XML file on disk. File is saved to the directory
#' named in \code{targetDirectory}.
#' 
#' @details The resulting XML file will be called \file{\code{module$name}.xml}
#' unless another \code{filename} is specified.
#' 
#' \code{targetDirectory} must exist, or function exits with error. If no
#' \code{targetDirectory} file is saved to current working directory.
#' 
#' @param module \code{module} object
#' @param targetDirectory destination directory
#' @param filename Filename for resulting XML file
#' @return resulting file location
#' @import XML
#' @export
#' 
#' @examples
#' 
#' targ1 <- tempdir() 
#' 
#' ## use a module's name for filename
#' mod1xml <- system.file("extdata", "simpleGraph", "createGraph.xml", 
#' 		           package = "conduit")
#' mod1 <- loadModule("createGraph", 
#' 		       ref = mod1xml)
#' saveModule(module = mod1, targetDirectory = targ1)
#' 
#' ## specify a filename for the module XML
#' mod2xml <- system.file("extdata", "simpleGraph", "layoutGraph.xml",
#' 		           package = "conduit")
#' mod2 <- loadModule("layoutGraph",
#' 		       ref = mod2xml)
#' saveModule(module = mod2, targetDirectory = targ1,
#' 	       filename = "myNewModule.xml")
saveModule <- function(module, targetDirectory=getwd(),
                       filename=paste0(module$name, ".xml")) {
    targetDirectory <- file.path(targetDirectory)
    if (!file.exists(targetDirectory)) {
        stop("no such target directory")
    }
    moduleDoc <-
        newXMLDoc(namespaces="http://www.openapi.org/2014",
                  node=moduleToXML(module,
                      namespaceDefinitions="http://www.openapi.org/2014/"))
    moduleFilePath <- file.path(targetDirectory, filename)
    saveXML(moduleDoc, moduleFilePath)
}

## RUNNING A MODULE

#' Execute a \code{module}'s \code{moduleSource}s in the specified
#' platform.
#'
#' @param module \code{module} object
#' @param inputs Names list of input locations
#' @param moduleFiles File path to module output location
runPlatform <- function(module, inputs, moduleFiles) {
    UseMethod("runPlatform")
}

#' Execute a \code{module}'s source(s)
#'
#' Execute the scripts contained in or referenced by a \code{module}'s sources.
#'
#' @details This function:
#' \itemize{
#'   \item creates a directory for the \code{module} output
#'   \item determines which platform the module requires
#'   \item executes the \code{module}'s source(s) using this platform
#' }
#'
#' If the \code{module} has inputs the \code{inputs} list must have a named
#' absolute file location for each input.
#'
#' \code{targetDirectory} must exist or the function will return an error.
#'
#' This function creates a directory called \sQuote{modules} in
#' the \code{targetDirectory} if it does not already exist.
#'
#' @param module \code{module} object
#' @param inputs Named list of input locations
#' @param targetDirectory File path for module output
#' @seealso \code{module}, \code{moduleSource}
#' @export
#'
#' @examples
#'
#' targ1 <- tempdir()
#' 
#' ## run a module with no inputs
#' mod1xml <- system.file("extdata", "simpleGraph", "createGraph.xml", 
#' 		       package = "conduit")
#' mod1 <- loadModule("createGraph", 
#' 		   ref = mod1xml)
#' runModule(module = mod1, targetDirectory = targ1)
#' 
#' ## run a module with inputs
#' mod2xml <- system.file("extdata", "simpleGraph", "layoutGraph.xml",
#' 		       package = "conduit")
#' mod2 <- loadModule("layoutGraph",
#' 		   ref = mod2xml)
#' ## mod1 output locations
#' names(mod1$outputs)
#' list.files(path = file.path(targ1, "modules", mod1$name),
#'            pattern = paste0("^directedGraph"), full.names = TRUE)
#' ## mod2 input names
#' names(mod2$inputs)
#' mod2inputs <- 
#'     list(myGraph = file.path(targ1, "modules", "createGraph", 
#'                              "directedGraph.rds"))
#' runModule(module = mod2, targetDirectory = targ1, inputs = mod2inputs)
runModule <- function(module, inputs=list(),
                      targetDirectory=getwd()) {
    targetDirectory <- file.path(targetDirectory)
    if (!file.exists(targetDirectory)) {
        stop("no such target directory")
    }
    moduleName <- module$name
    ## create a directory for this module's output
    modulePath <- file.path(targetDirectory, "modules", moduleName)
    if (file.exists(modulePath))
        unlink(modulePath, recursive=TRUE)
    dir.create(modulePath, recursive=TRUE)
    moduleFiles <- tools::file_path_as_absolute(modulePath)

    ## set the module class to PLATFORM
    modulePlatform <- module$platform
    class(module) <- modulePlatform

    ## run this module with the appropriate Platform Support
    runPlatform(module, inputs, modulePath)
}

## module creation functions

#' Create a \code{module} platform node
#'
#' @param name Name of platform
#' @return A named character vector containing the platform name
#' @seealso \code{module}
modulePlatform <- function(name) {
    c(name=name)
}

#' Create a \code{module} input
#'
#' Creates a \code{moduleInput} vector for use in a \code{module}'s inputs list.
#'
#' \code{type} must be \sQuote{internal} or \sQuote{external}.
#'
#' @param name Input name
#' @param type \sQuote{internal} or \sQuote{external}
#' @param format Input format
#' @param formatType Defaults to \dQuote{text}
#' @return named \code{moduleInput} character vector containing:
#' \itemize{
#'   \item{name}
#'   \item{type}
#'   \item{format}
#'   \item{formatType}
#' }
#' @seealso \code{module}
#' @export
#'
#' @examples
#'
#' inp1 <- moduleInput(name = "bigData", type = "internal",
#'                     format = "R data frame")
moduleInput <- function(name, type, format="", formatType="text") {
    ## fail if type is not 'internal' or 'external'
    if (!(type == "internal" || type == "external")) {
        stop(paste0("specified type '", type, "' is not supported"))
    }
    inp <- c(name=name, type=type, format=format, formatType=formatType)
    class(inp) <- "moduleInput"
    inp
}

#' Create a \code{module} output input
#'
#' Creates a \code{moduleOutput} vector for use in a \code{module}'s outputs
#' list.
#'
#' @details \code{type} must be \sQuote{internal} or \sQuote{external}.
#'
#' It \code{type} is \dQuote{external}, a \code{ref} is required. This needs
#' to be a resolveable URI created by the \code{module}'s source(s).
#'
#' @param name Output name
#' @param type \sQuote{internal} or \sQuote{external}
#' @param format Output format
#' @param formatType Defaults to \dQuote{text}
#' @param ref Filename of \sQuote{external} output
#' @return named \code{moduleOutput} character vector of:
#' \itemize{
#'   \item{name}
#'   \item{type}
#'   \item{format}
#'   \item{formatType}
#'   \item{ref}
#' }
#' @seealso \code{module}
#' @export
#'
#' @examples
#' outp1 <- moduleOutput(name = "bigData", type = "internal",
#'                       format = "R data frame")
#' outp2 <- moduleOutput(name = "mediumData", type = "external",
#'                       format = "CSV file", ref = "mediumData.csv")
moduleOutput <- function(name, type, format="", formatType="text", ref="") {
    ## fail if type is not 'internal' or 'external'
    if (!(type == "internal" || type == "external")) {
        stop(paste0("specified type '", type, "' is not supported"))
    }
    ## fail if no 'ref' set for "external" type
    if (type == "external" && ref == "") {
        stop(paste0("no 'ref' set for \"external\" input '", name, "'"))
    }
    outp <- c(name=name, type=type, format=format, formatType=formatType,
              ref=ref)
    class(outp) <- "moduleOutput"
    outp
}

#' Create a \code{module} source
#'
#' Creates a \code{moduleSource} vector for use in a \code{module}'s sources
#' list.
#'
#' @details If a \code{ref} is provided the returned value will be read from
#' the script file found using \code{ref} and \code{path}. Otherwise the
#' \code{value} is used.
#'
#' \code{module} sources are exectuted in the order determined by each source's
#' \sQuote{order}. Running order is:
#' \enumerate{
#'   \item{negative numbers in ascending order}
#'   \item{zero}
#'   \item{no order specified}
#'   \item{positive numbers in ascending order}
#' }
#'
#' @param value source script
#' @param ref module XML filename
#' @param path search path(s) (optional)
#' @param type not used as at 2014-12-05
#' @param order character containing numeric value specifying source position in sources
#' @return named \code{moduleSource} list containing:
#' \itemize{
#'   \item{value: source script}
#'   \item{type: not used as at 2014-12-05}
#'   \item{order: numeric value determining position of source in sources}
#'   \item{ref: originating XML filename}
#'   \item{path: originating search path}
#' }
#' @seealso \code{module}
#' @export
#' @examples
#' ## create moduleSource with source script in 'value'
#' val1 <- c("x <- 1:10", "y <- rnorm(10, 0, 1)", "plot(x, y)")
#' src1 <- moduleSource(value = val1, order = "-1")
#'
#' ## create a moduleSource with source script given by 'ref'
#' modScript <- system.file("extdata", "simpleGraphScripts", "createGraph.R",
#'                          package = "conduit")
#' src2 <- moduleSource(ref = modScript)
moduleSource <- function(value, ref=NULL, path=defaultSearchPaths, type="",
                         order="") {
    if (!is.null(ref)) {
        ## FIXME: not properly teste
        ## FIXME: ignores the possibility of creating a source given by ref
        value <-
            tryCatch(fetchRef(ref, path),
                     error = function(err) {
                         stop("Unable to read module source\n",
                              err)
                     })
    }
    list(value=value, type=type, order=order, ref=ref, path=path)
}

#' Create a \code{module} object
#'
#' Creates a module object which can be executed in conduit.
#'
#' @details \code{inputs}, \code{outputs}, and \code{sources} should be lists
#' of objects created using \code{moduleInput}, \code{moduleOutput}, and
#' \code{moduleSource} respectively.
#'
#' \code{path} optionally specifies search path(s) to be used for any of the
#' module's children, e.g. a source specified given by \sQuote{ref}.
#'
#' @param name Name of module
#' @param platform Platform name
#' @param description A basic description of the module
#' @param inputs List of \code{moduleInput} objects
#' @param outputs List of \code{moduleOutput} objects
#' @param sources List of \code{moduleSource} objects
#' @param path Search path(s) for module children (optional)
#' @return \code{module} list containing:
#' \itemize{
#'   \item{name}
#'   \item{platform}
#'   \item{description}
#'   \item{inputs}
#'   \item{outputs}
#'   \item{sources}
#'   \item{path}
#' }
#' @seealso \code{moduleInput}, \code{moduleOutput} and \code{moduleSource} for
#' creating object for these lists. \code{loadModule} for reading a module
#' from an XML file. \code{runModule} for executing a module's source
#' scripts.
#' @export
#' @examples
#' ## create a module with one output and one source
#' src1 <- moduleSource(value = "x <- \"set\"")
#' outp1 <- moduleOutput(name = "x", type = "internal",
#'                       format = "R character string")
#' mod1 <- module(name = "setX", platform = "R",
#'                description = "sets the value of x",
#'                outputs = list(outp1),
#'                sources = list(src1))
#'
#' ## create a module with one input and one source
#' mod2 <- module("showY", platform = "R",
#'                description = "displays the value of Y",
#'                inputs = list(moduleInput(name = "y", type = "internal",
#'                                          format = "R character string")),
#'                sources = list(moduleSource(value = "print(y)")))
module <- function(name, platform, description="", inputs=NULL,
                   outputs=NULL, sources=list(), path=NULL) {
    platform <- modulePlatform(platform)
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
    module <- list(name=name, platform=platform, description=description,
                   inputs=inputs, outputs=outputs,
                   sources=sources, path=path)
    class(module) <- "module"
    module
}
