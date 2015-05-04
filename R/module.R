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

#' Parse module XML and return a module object
#'
#' @param name module name
#' @param xml module \code{XMLNode}
#' @param location file directory of invoking pipeline/module xml (optional)
#' @return \code{module} object
#' @import XML
readModuleXML <- function(name, xml, location = getwd()) {
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
               function(node, location) {
                   attrs <- xmlAttrs(node)
                   type <- getXMLAttr(node, "type")
                   order <- getXMLAttr(node, "order")
                   ref <- getXMLAttr(node, "ref")
                   path <- getXMLAttr(node, "path")
                   value <-
                       if (is.null(ref)) {
                           xmlValue(node)
                       } else {
                           ## FIXME: not well tested or even understood
                           file <- tryCatch(
                               resolveRef(ref, path, location),
                               error = function (err) {
                                   stop(paste0("Unable to load module source at ref: ", ref, ", path: ", path, "\n"),
                                        err)
                               })
                           fetchRef(file)
                       }
                   list(value=value, type=type, order=order, ref=ref,
                        path=path)
               }, location)
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
    module(name=name, description=description,
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
#' @export
loadModule <- function(name, ref, path = NULL,
                       namespaces=c(oa="http://www.openapi.org/2014/")) {
    ## if path is not set, make path from ref
    if (is.null(path)) {
        path <- paste0(dirname(ref), pathSep)
        ref <- basename(ref)
    }
    ## fetch module XML from disk
    file <- tryCatch(
        resolveRef(ref, path),
        error = function(err) {
            problem <- c(paste0("Unable to load module '", name, "'\n"),
                         err)
            stop(problem)
        })
    location <- dirname(file)
    rawXML <- fetchRef(file)
    xml <- xmlRoot(xmlParse(rawXML))
    module <- readModuleXML(name, xml, location)
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
                   value <- s$value
                   ref <- s$ref
                   path <- s$path
                   type <- s$type
                   order <- s$order
                   
                   ## create new source node
                   sourceNode <- newXMLNode(name = "source")
                   
                   ## if no ref is given, save 'value' inline as cdata
                   if (is.null(ref)) {
                       sourceNode <-
                           addChildren(
                               node = sourceNode,
                               ## collapse value script with newline
                               ## (assumes value is character vector)
                               kids = paste0(value, collapse = "\n"),
                               cdata = TRUE)
                   } else {
                       ## else record ref and path as attrs
                       xmlAttrs(sourceNode) <- c(ref=ref, path=path)
                   }
                   
                   ## set source 'type' if not NULL
                   if (!is.null(type)) {
                       xmlAttrs(sourceNode) <- c(type=type)
                   }
                   
                   ## set source 'order' if not NULL
                   if (!is.null(order)) {
                       xmlAttrs(sourceNode) <- c(order=order)
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
    moduleFiles <- normalizePath(modulePath)

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
#' 
#' @return \code{modulePlatform} list object
#' 
#' @seealso \code{module}
modulePlatform <- function(name) {
    if (!is_length1_char(name)) {
        stop("platform 'name' is not a length 1 character vector")
    }
    modulePlatform <- list(name = name)
    class(modulePlatform) <- "modulePlatform"
    return(modulePlatform)
}

#' Create an \code{ioFormat} object.
#'
#' Specify the format of a \code{moduleInput} or \code{moduleOutput} object.
#'
#' If \code{type} = \dQuote{text}, \code{value} must be a character
#' vector of length 1.
#'
#' @param value Format information
#' @param type Method of format specification
#'
#' @return \code{ioFormat} list object
#'
#' @seealso \code{moduleInput}, \code{moduleOutput}, \code{module}
#'
#' @examples
#' i1_format <- ioFormat(value = "CSV file")
#'
#' @export
ioFormat <- function(value, type="text") {
    if (!is_length1_char(type)) {
        stop("'type' is not a length 1 character")
    }
    ## give error if value doesn't match format, or format not defined
    switch(type,
           text = if(!is_length1_char(value)) {
               stop("'value' is not a length 1 character vector")
           },
           stop("invalid 'type' provided")
           )
    ioFormat <- list(value = value, type = type)
    class(ioFormat) <- "ioFormat"
    return(ioFormat)
}

#' Create a \code{module} input
#'
#' Create a \code{moduleInput} list for use in a \code{module}'s inputs list
#'
#' \code{vessel} cannot be a \code{scriptVessel} object, as these are not
#' defined for \code{moduleIO} objects.
#'
#' @param name Input name
#' @param vessel \code{vessel} object
#' @param format \code{ioFormat} object
#' 
#' @return named \code{moduleInput} list containing:
#' \itemize{
#'   \item{name}
#'   \item{vessel}
#'   \item{format}
#' }
#' @seealso \code{module}
#'
#' @examples
#'
#' internalInput <-
#'     moduleInput(
#'         name = "bigData",
#'         vessel = internalVessel(symbol = "bigData"),
#'         format = ioFormat("R data frame"))
#' fileInput <-
#'     moduleInput(
#'         name = "scores.csv",
#'         vessel = fileVessel(ref = "2015-03-13-scores.csv"),
#'         format = ioFormat("CSV file"))
#' 
#' @export
moduleInput <- function(name, vessel, format) {
    moduleInput <- moduleIO(name = name, type = "input",
                            vessel = vessel, format = format)
    class(moduleInput) <- c("moduleInput", class(moduleInput))
    return(moduleInput)
}

#' Create a \code{module} output
#'
#' Create a \code{moduleOutput} list for use in a \code{module}'s
#' outputs list.
#'
#' \code{vessel} cannot be a \code{scriptVessel} object, as these are not
#' defined for \code{moduleIO} objects.
#'
#' @param name Output name
#' @param vessel \code{vessel} object
#' @param format \code{ioFormat} object
#' 
#' @return named \code{moduleOutput} list containing:
#' \itemize{
#'   \item{name}
#'   \item{vessel}
#'   \item{format}
#' }
#' @seealso \code{module}
#'
#' @examples
#'
#' internalOutput <-
#'     moduleOutput(
#'         name = "bigData",
#'         vessel = internalVessel(symbol = "bigData"),
#'         format = ioFormat("R data frame"))
#' fileInput <-
#'     moduleOutput(
#'         name = "scores.csv",
#'         vessel = fileVessel(ref = "2015-03-13-scores.csv"),
#'         format = ioFormat("CSV file"))
#' 
#' @export
moduleOutput <- function(name, vessel, format) {
    moduleOutput <- moduleIO(name = name, type = "output",
                            vessel = vessel, format = format)
    class(moduleOutput) <- c("moduleOutput", class(moduleOutput))
    return(moduleOutput)
}

#' Create a \code{module} input or output object.
#'
#' @details This function is used by \code{moduleInput} and
#' \code{moduleOutput} to create input and output objects for
#' modules.
#'
#' \code{vessel} cannot be a \code{scriptVessel} object, as these are not
#' defined for moduleIO objects.
#'
#' @param name Input/output name
#' @param type \dQuote{input} or \dQuote{output}
#' @param vessel \code{vessel} object
#' @param format \code{ioFormat} object
#'
#' @return \code{moduleIO} object
#'
#' @seealso \code{module}, \code{moduleInput}, \code{moduleOutput},
#' \code{ioFormat}, \code{vessel}
moduleIO <- function(name, type, vessel, format) {
    if (!is_length1_char(name)) {
        stop("'name' is not a length 1 character vector")
    }
    if (!is_length1_char(type)) {
        stop("'type' is not a length 1 character vector")
    }
    if (!("vessel" %in% class(vessel))) {
        stop("'vessel' is not a 'vessel' object")
    }
    if ("scriptVessel" %in% class(vessel)) {
        stop("'scriptVessel' vessels not defined for moduleIO objects")
    }
    if (!("ioFormat" %in% class(format))) {
        stop("'format' is not an 'ioFormat' object")
    }
    ## fail if 'type' not "input" or "output"
    if (!(type %in% c("input", "output"))) {
        stop("'type' must be \"input\" or \"output\"")
    }

    moduleIO <- list(name = name, type = type,
                     vessel = vessel, format = format)
    class(moduleIO) <- "moduleIO"
    return(moduleIO)
}

#' Create a \code{module} source
#'
#' Create a \code{moduleSource} object for use in a \code{module}'s sources
#' list.
#'
#' @details The contents of a module's source script are provided by
#' the \code{vessel} object. For inline code use a \code{scriptVessel}
#' object. For a script file on the local filesystem use
#' \code{fileVessel}.
#'
#' \code{vessel} cannot be an \code{inputVessel} object, as these are not
#' defined for moduleIO objects.
#'
#' \code{module} sources are exectuted in the order determined by each
#' source's \sQuote{order}.
#'
#' Running order is:
#' \enumerate{
#'   \item{negative numbers in ascending order}
#'   \item{zero}
#'   \item{no order specified}
#'   \item{positive numbers in ascending order}
#' }
#'
#' @param vessel \code{vessel} object
#' @param order numeric value specifying source position in sources
#' 
#' @return named \code{moduleSource} list containing:
#' \itemize{
#'   \item{vessel: \code{vessel} object}
#'   \item{order: numeric value determining position of source in sources}
#' }
#' 
#' @seealso \code{module}, \code{fileVessel}, \code{scriptVessel}
#' 
#' @examples
#' ## create moduleSource with source script inline
#' val1 <- c("x <- 1:10", "y <- rnorm(10, 0, 1)", "plot(x, y)")
#' src1 <- moduleSource(vessel = scriptVessel(value = val1),
#'                      order = -1)
#'
#' ## create a moduleSource with source script from file
#' modScript <- system.file("extdata", "simpleGraphScripts", "createGraph.R",
#'                          package = "conduit")
#' src2 <- moduleSource(vessel = fileVessel(ref = modScript))
#'
#' @export
moduleSource <- function(vessel, order = NULL) {
    if (!("vessel" %in% class(vessel))) {
        stop("'vessel' is not a vessel object")
    }
    if ("internalVessel" %in% class(vessel)) {
        stop("'internalVessel' vessels not defined for moduleSource objects")
    }
    if (!is.null(order)) {
        if (!is.numeric(order)) {
            stop("'order' is not numeric")
        } else if (length(order) > 1) {
            stop("more than one value given for 'order'")
        }
    }
    src <- list(vessel = vessel, order = order)
    class(src) <- "moduleSource"
    src
}

#' Create a \code{module} object
#'
#' Creates a module object which can be executed in conduit.
#'
#' @details \code{inputs}, \code{outputs}, and \code{sources} should be lists
#' of objects created using \code{moduleInput}, \code{moduleOutput}, and
#' \code{moduleSource} respectively.
#'
#' @param name Name of module
#' @param platform Platform name
#' @param host Machine on which module is to be run
#' @param description A basic description of the module
#' @param inputs List of \code{moduleInput} objects
#' @param outputs List of \code{moduleOutput} objects
#' @param sources List of \code{moduleSource} objects
#' 
#' @return \code{module} list containing:
#' \itemize{
#'   \item{name}
#'   \item{platform}
#'   \item{host}
#'   \item{description}
#'   \item{inputs}
#'   \item{outputs}
#'   \item{sources}
#' }
#' 
#' @seealso \code{moduleInput}, \code{moduleOutput} and
#' \code{moduleSource} for creating objects for these
#' lists. \code{loadModule} for reading a module from an XML
#' file. \code{runModule} for executing a module's source scripts.
#' 
#' @examples
#' ## create a module with one output and one source
#' src1 <- moduleSource(vessel = scriptVessel(value = "x <- \"set\""))
#' outp1 <- moduleOutput(
#'              name = "x",
#'              internalVessel(symbol = "x"),
#'              format = ioFormat("R character string"))
#' mod1 <- module(name = "setX", platform = "R",
#'                description = "sets the value of x",
#'                outputs = list(outp1),
#'                sources = list(src1))
#' 
#' ## create a module with one input and one source
#' mod2 <-
#'     module(
#'         "showY",
#'         platform = "R",
#'         host = "localhost",
#'         description = "displays the value of Y",
#'         inputs =
#'             list(
#'                 moduleInput(
#'                     name = "y",
#'                     vessel = internalVessel(symbol = "y"),
#'                     format = ioFormat("R character string"))),
#'         sources =
#'             list(
#'                 moduleSource(
#'                 scriptVessel(value = "print(y)"))))
#' 
#' @export
module <- function(name, platform, host=NULL,
                   description=NULL,
                   inputs=NULL, outputs=NULL, sources=NULL) {
    ## check arguments for errors

    ## check 'name'
    if (!is_length1_char(name)) {
        stop("'name' is not a length 1 character vector")
    }

    ## check 'platform'
    platform <-
        tryCatch(
            modulePlatform(platform),
            error =
                function(e) {
                    e <- paste("problem creating modulePlatform: \n", e)
                    stop(e)
                })

    ## check 'host'
    if (!is.null(host)) {
        if (!is_length1_char(host)) {
            stop("'host' is not a length 1 character vector")
        }
    }

    ## check 'description'
    if (!is.null(description)) {
        if (!is.character(description)) {
            stop("'description' is not a character object")
        }
    }

    ## check 'inputs'
    if (!is.null(inputs)) {
        if (class(inputs) != "list") {
            stop("'inputs' is not a list object")
        }
        inputClasses <- lapply(inputs, class)
        for (i in seq_along(inputClasses)) {
            if (inputClasses[[i]][1] != "moduleInput") {
                stop(paste0("input ", i, " is not a 'moduleInput' object"))
            }
        }
        ## name inputs
        names(inputs) <-
            sapply(inputs,
                   function (x) {
                       x["name"]
                   })
    }

    ## check 'outputs'
    if (!is.null(outputs)) {
        if (class(outputs) != "list") {
            stop("'outputs' is not a list object")
        }
        outputClasses <- lapply(outputs, class)
        for (i in seq_along(outputClasses)) {
            if (outputClasses[[i]][1] != "moduleOutput") {
                stop(paste0("output ", i, " is not a 'moduleOutput' object"))
            }
        }        
        ## name outputs
        names(outputs) <-
            sapply(outputs,
                   function (x) {
                       x["name"]
                   })
    }

    ## check 'sources'
    if (!is.null(sources)) {
        if (class(sources) != "list") {
            stop("'sources' is not a list object")
        }
        sourceClasses <- lapply(sources, class)
        for (i in seq_along(sourceClasses)) {
            if (sourceClasses[[i]][1] != "moduleSource") {
                stop(paste0("source ", i, " is not a 'moduleSource' object"))
            }
        }                
    }
    
    module <- list(name = name,
                   platform = platform,
                   host = host,
                   description = description,
                   inputs = inputs,
                   outputs = outputs,
                   sources = sources)
    class(module) <- "module"
    module
}
