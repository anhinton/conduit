#' Create a \code{module} object
#'
#' Creates a module object which can be executed in conduit.
#'
#' \code{inputs}, \code{outputs}, and \code{sources} should be lists
#' of objects created using \code{moduleInput}, \code{moduleOutput},
#' and \code{moduleSource} respectively.
#'
#' Module \code{location} defaults to current working
#' directory. This can be set to indicate the location of the module
#' XML file, and its supporting files.
#'
#' If \code{moduleHost} is not provided module will be executed on
#' local machine.
#'
#' @param name Name of module
#' @param language Language name
#' @param host \code{moduleHost} object describing machine where
#'     module is to be executed
#' @param description A basic description of the module
#' @param inputs List of \code{moduleInput} objects
#' @param outputs List of \code{moduleOutput} objects
#' @param sources List of \code{moduleSource} objects
#' @param location file directory where module xml and files are found
#' 
#' @return \code{module} list containing:
#'
#' \item{name}{module name}
#' \item{language}{module language}
#' \item{host}{\code{moduleHost} object}
#' \item{description}{module description}
#' \item{inputs}{list of \code{moduleInput} objects}
#' \item{outputs}{list of \code{moduleOutput} objects}
#' \item{sources}{list of \code{moduleSource} objects}
#' 
#' @seealso \code{moduleInput}, \code{moduleOutput} and
#' \code{moduleSource} for creating objects for these
#' lists. \code{loadModule} for reading a module from an XML
#' file. \code{saveModule} for saving a module as an XML
#' file. \code{runModule} for executing a module's source scripts.
#' 
#' @examples
#' ## create a module with one output and one source
#' src1 <- moduleSource(vessel = scriptVessel(value = "x <- \"set\""))
#' outp1 <- moduleOutput(
#'              name = "x",
#'              internalVessel(symbol = "x"),
#'              format = ioFormat("R character string"))
#' mod1 <- module(name = "setX", language = "R",
#'                description = "sets the value of x",
#'                outputs = list(outp1),
#'                sources = list(src1))
#' 
#' ## create a module with one input and one source
#' mod2 <-
#'     module(
#'         "showY",
#'         language = "R",
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
module <- function(name, language, host=NULL,
                   description=NULL,
                   inputs=NULL, outputs=NULL, sources=NULL,
                   location = getwd()) {
    ## check arguments for errors

    ## check 'name'
    if (!is_length1_char(name)) {
        stop("'name' is not a length 1 character vector")
    }

    ## check 'language'
    if (!is.null(language)) {
        if (!is_length1_char(language))
            stop("'language' is not a length 1 character vector")
    }
    
    ## check 'host'
    if (!is.null(host)) {
        if (!inherits(host, "moduleHost"))
            stop("'host' is not moduleHost object")
    }

    ## check 'description'
    if (!is.null(description)) {
        if (!is.character(description))
            stop("'description' is not a character object")
    }

    ## check 'inputs'
    if (!is.null(inputs)) {
        if (class(inputs) != "list")
            stop("'inputs' is not a list object")
        if (!all(sapply(inputs, inherits, "moduleInput")))
            stop("inputs must be moduleInput objects")
        names(inputs) <- sapply(inputs, getName)
    }

    ## check 'outputs'
    if (!is.null(outputs)) {
        if (class(outputs) != "list")
            stop("'outputs' is not a list object")
        if (!all(sapply(outputs, inherits, "moduleOutput")))
            stop("outputs must be moduleOutput objects")
        names(outputs) <- sapply(outputs, getName)
    }

    ## check 'sources'
    if (!is.null(sources)) {
        if (class(sources) != "list")
            stop("'sources' is not a list object")
        if (!all(sapply(sources, inherits, "moduleSource")))
            stop("sources must be moduleSource objects")
    }
    
    module <- list(name = name,
                   language = language,
                   host = host,
                   description = description,
                   inputs = inputs,
                   outputs = outputs,
                   sources = sources)
    class(module) <- "module"
    attr(module, "location") <- location
    module
}

#' \code{moduleHost} object
#'
#' @seealso \code{vagrantHost}, \code{module}
#'
#' @name moduleHost
NULL

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
    ## give error if value doesn't match format, or format not defined.
    ## this serves as a master list of know formatTypes
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
    class(moduleIO) <-
        switch(type,
               input = c("moduleInput", "moduleIO"),
               output = c("moduleOutput", "moduleIO"))
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

#' @describeIn getName
#'
#' Returns module name
#'
#' @export
getName.module <- function(x) {
    x$name
}

#' @describeIn getDescription
#'
#' Returns module description
#'
#' @export
getDescription.module <- function(x) {
    x$description
}

#' @describeIn getLanguage
#'
#' Returns module language
#'
#' @export
getLanguage.module <- function(x) {
    x$language
}

#' @describeIn getLocation
#'
#' Returns location of module XML file
#'
#' @export
getLocation.module <- function(x) {
    attr(x, "location")
}

#' Load a module from an XML file
#'
#' Reads an XML file given by \code{ref} and \code{path} and interprets to
#' produce a \code{module}.
#'
#' If the module XML file is not valid OpenAPI module XML this
#' function will return an error.
#'
#' If \code{path} is not set and conduit needs to search for the file
#' the default search paths are used.
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
    ## fetch module XML from disk
    rawXML <- tryCatch(
        fetchVessel(fileVessel(ref, path)),
        error = function(err) {
            problem <- c(paste0("Unable to load module '", name, "'\n"),
                         err)
            stop(problem)
        })
    if (!isValidXML(rawXML, "module"))
        stop(paste0("'", ref, "': module XML is invalid"))
    xml <- xmlRoot(xmlParse(rawXML))

    ## create module object
    module <- readModuleXML(name, xml)

    ## store location of originating module file
    attr(module, "location") <- attr(rawXML, "location")
    
    return(module)
}

#' Create a \code{moduleHost} object from host XML
#'
#' This function creates a \code{moduleHost} object from valid host
#' elements.
#'
#' As of 2016-02-26 only \samp{<vagrant/>} elements are
#' supported.
#'
#' @param moduleHostXML host XML node
#'
#' @return \code{moduleHost} object
#'
#' @import XML
readModuleHostXML <- function(moduleHostXML) {
    type <- xmlName(moduleHostXML)
    moduleHost <- switch(
        type,
        vagrant = readVagrantHostXML(moduleHostXML),
        docker = readDockerHostXML(moduleHostXML)
    )
    if(!inherits(moduleHost, "moduleHost"))
        class(moduleHost) <- c(class(moduleHost), "moduleHost")
    moduleHost
}

#' Create a \code{vessel} object from vessel XML
#'
#' @param xml vessel XML
#'
#' @return \code{vessel} object
#'
#' @import XML
readVesselXML <- function (xml) {
    type <- xmlName(xml)
    content <-
        switch(type,
               script = xmlValue(xml),
               internal = xmlAttrs(xml),
               file = xmlAttrs(xml),
               url = xmlAttrs(xml),
               stop("'vessel' xml unknown type"))    
    vessel <-
        switch(type,
               file = {
                   ref = content[["ref"]]
                   path = if ("path" %in% names(content)) {
                       content[["path"]]
                   } else {
                       NULL
                   }
                   fileVessel(ref = ref, path = path)
               },
               internal = internalVessel(
                   symbol = content[["symbol"]]),
               url = urlVessel(
                   ref = content[["ref"]]),
               script = scriptVessel(readLines(textConnection(content))))
    return(vessel)
}

#' Create a \code{ioFormat} object from format XML
#'
#' @param xml format XML
#'
#' @return \code{ioFormat} object
#'
#' @import XML
readIOFormatXML <- function (xml) {
    if (xmlName(xml) != "format") {
        stop("ioFormat XML is invalid")
    }   
    value <- xmlValue(xml)
    attrs <- xmlAttrs(xml)
    ioFormat <-
        if (is.null(attrs)) {
            ioFormat(value = value)
        } else {
            ioFormat(value = value, type = attrs[["formatType"]])
        }
    return(ioFormat)
}

#' Create a \code{moduleIO} object from input/output XML
#'
#' @param xml input/output XML
#'
#' @return \code{moduleIO} object
#'
#' @import XML
readModuleIOXML <- function (xml) {
    type <- xmlName(xml)
    if (type != "input" && type != "output") {
        stop("moduleIO XML is invalid")
    }
    name <- xmlAttrs(xml)[["name"]]
    children <- xmlChildren(xml)

    ## create ioFormat object
    formatChild <- children$format
    ioFormat <- readIOFormatXML(formatChild)

    ## create vessel object:
    ## determine which child has an appropriate vessel name
    vesselChild <-
        which(names(children) %in% c("internal", "file", "url"))
    vessel <- readVesselXML(children[[vesselChild]])

    ## create moduleIO object
    moduleIO <- moduleIO(name = name, type = type, vessel = vessel,
                         format = ioFormat)
    return(moduleIO)
}

#' create a \code{moduleSource} object from module source XML
#'
#' @param xml module source XML
#'
#' @return \code{moduleSource} object
#'
#' @import XML
readModuleSourceXML <- function (xml) {
    if (xmlName(xml) != "source") {
        stop("moduleSource XML is invalid")
    }

    ## extract vessel object.
    child <- xmlChildren(xml)[[1]] # there should be only one child
    vessel <- readVesselXML(child)

    attrs <- xmlAttrs(xml)
    moduleSource <-
        if (is.null(attrs)) {
            moduleSource(vessel = vessel)
        } else {
            moduleSource(vessel = vessel, order = as.numeric(attrs[["order"]]))
        }
    return(moduleSource)
}

#' Parse module XML and return a \code{module} object
#'
#' @param name module name
#' @param xml module \code{XMLNode}
#' @param location file directory of invoking pipeline/module xml (optional)
#' 
#' @return \code{module} object
#' 
#' @import XML
readModuleXML <- function (name, xml, location = getwd()) {
    attrs <- xmlAttrs(xml)
    language <- attrs[["language"]]
    nodes <- xmlChildren(xml)
    nodeNames <- names(nodes)

    ## extract host
    host <-
        if ("host" %in% nodeNames) {
            hostNode <- nodes$host
            moduleHostXML <- xmlChildren(hostNode)[[1]]
            readModuleHostXML(moduleHostXML)
        } else {
            NULL
        }
    
    ## extract description
    description <-
        if ("description" %in% nodeNames) {
            descNode <- nodes$description
            xmlValue(descNode)
        } else {
            NULL
        }
    
    ## extract inputs
    inputNodes <- nodes[names(nodes) == "input"]
    inputs <-
        if (length(inputNodes) == 0) {
            NULL
        } else {
            lapply(inputNodes, readModuleIOXML)
        }
    
    ## extract sources
    sourceNodes <- nodes[names(nodes) == "source"]
    sources <-
        if (!length(sourceNodes)) {
            NULL
        } else {
            lapply(sourceNodes, readModuleSourceXML)
        }
    
    ## extract outputs
    outputNodes <- nodes[names(nodes) == "output"]
    outputs <-
        if (length(outputNodes) == 0) {
            NULL
        } else {
            lapply(outputNodes, readModuleIOXML)
        }

    module(name = name,
           language = language,
           host = host,
           description = description,
           inputs = inputs,
           sources = sources,
           outputs = outputs,
           location = location)
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
saveModule <- function(module, targetDirectory = getwd(),
                       filename = paste0(module$name, ".xml")) {
    targetDirectory <- file.path(targetDirectory)
    if (!file.exists(targetDirectory)) {
        stop("no such target directory")
    }
    namespace <- "http://www.openapi.org/2014/"
    moduleXML <- moduleToXML(module, namespaceDefinitions = namespace)
    moduleDoc <- newXMLDoc(
        namespaces = namespace,
        node = moduleXML)
    moduleFilePath <- file.path(targetDirectory, filename)
    saveXML(moduleDoc, moduleFilePath)
}

#' Convert a \code{module} object to XML
#'
#' @param module \code{module} object
#' @param namespaceDefinitions XML namespaces as character vector
#' 
#' @return \code{XMLInternalNode} object
#'
#' @seealso \code{module} objects
#' 
#' @import XML
moduleToXML <- function (module,
                         namespaceDefinitions=NULL) {
    if (class(module) != "module") {
        stop("'module' is not a 'module' object")
    }
    moduleRoot <- newXMLNode(name = "module",
                             attrs = c(language = module$language),
                             namespaceDefinitions = namespaceDefinitions)
    host <-
        if (!is.null(module$host)) {
            moduleHostToXML(module$host)
        } else {
            NULL
        }
    description <-
        if (!is.null(module$description)) {
            newXMLNode("description", children = module$description)
        } else {
            NULL
        }
    inputs <- lapply(module$inputs, moduleIOToXML)
    outputs <- lapply(module$outputs, moduleIOToXML)
    sources <- lapply(module$sources, moduleSourceToXML)
    addChildren(moduleRoot,
                kids=list(host, description, inputs, sources,
                          outputs))
}

#' Create XML corresponding to a \code{moduleHost} object
#'
#' @param moduleHost \code{moduleHost} object
#'
#' @return XML node representing module host
moduleHostToXML <- function(moduleHost) {
    if (!inherits(moduleHost, "moduleHost"))
        stop("moduleHost object required")
    UseMethod("moduleHostToXML")
}

#' Create XML corresponding to a \code{vessel} object.
#'
#' @param vessel \code{vessel} object
#' @param namespaceDefinitions XML namespaces as character vector
#'
#' @return \code{XMLInternalNode} object
#'
#' @seealso \code{vessel} objects
#' 
#' @import XML
vesselToXML <- function (vessel,
                         namespaceDefinitions=NULL) {
    if (!("vessel" %in% class(vessel))) {
        stop("'vessel' is not a 'vessel' object")
    }
    
    ## determine vessel type from known list
    type <- switch(
        class(vessel)[1],
        fileVessel = "file",
        internalVessel = "internal",
        urlVessel = "url",
        scriptVessel = "script",
        stop("'vessel' is of unknown type")) # if vessel type not recognised

    ## create vessel XML object
    vesselXML <- newXMLNode(name = type,
                            namespaceDefinitions=namespaceDefinitions)

    ## assign value/attributes
    if (type == "script") {
        xmlChildren(vesselXML) <- newXMLCDataNode(vessel$value)
    } else {
        attributes <- unlist(vessel)
        xmlAttrs(vesselXML) <- attributes
    }
    
    return(vesselXML)
}
    
#' Create XML corresponding to an \code{ioFormat} object
#'
#' @param ioFormat \code{ioFormat} object
#' @param namespaceDefinitions XML namespaces as character vector
#'
#' @return \code{XMLInternalNode} object
#'
#' @seealso \code{ioFormat} objects
#'
#' @import XML
ioFormatToXML <- function (ioFormat,
                           namespaceDefinitions=NULL) {
    if (class(ioFormat) != "ioFormat") {
        stop("'ioFormat' is not an 'ioFormat' object")
    }
    ioFormatXML <- newXMLNode("format",
                              namespaceDefinitions=namespaceDefinitions)
    xmlAttrs(ioFormatXML) <- c("formatType" = ioFormat$type)
    xmlChildren(ioFormatXML) <- newXMLTextNode(ioFormat$value)
    return(ioFormatXML)
}

#' Create XML corresponding to \code{moduleIO} object
#'
#' @param moduleIO \code{moduleIO} object
#' @param namespaceDefinitions XML namespaces as character vector
#'
#' @return \code{XMLInternalNode} object
#'
#' @seealso \code{moduleIO} objects
#'
#' @import XML
moduleIOToXML <- function (moduleIO,
                           namespaceDefinitions = NULL) {
    if (!("moduleIO" %in% class(moduleIO))) {
        stop("'moduleIO' is not a 'moduleIO' object")
    }
    moduleIOXML <- newXMLNode(name = moduleIO$type,
                              attrs = c(name = moduleIO$name),
                              namespaceDefinitions = namespaceDefinitions)
    vesselXML <- vesselToXML(moduleIO$vessel, 
                             namespaceDefinitions = namespaceDefinitions)
    ioFormatXML <- ioFormatToXML(moduleIO$format,
                                 namespaceDefinitions = namespaceDefinitions)
    xmlChildren(moduleIOXML) <- list(vesselXML,
                                     ioFormatXML)
    return(moduleIOXML)
}

#' Create XML corresponding to \code{moduleSource} object
#'
#' @param moduleSource \code{moduleSource} object
#' @param namespaceDefinitions XML namespaces as character vector
#'
#' @return \code{XMLInternalNode} object
#'
#' @seealso \code{moduleSource} objects
#'
#' @import XML
moduleSourceToXML <- function (moduleSource,
                               namespaceDefinitions = NULL) {
    if (class(moduleSource) != "moduleSource") {
        stop("'moduleSource' is not a 'moduleSource' object")
    }
    moduleSourceXML <- newXMLNode(name = "source",
                                  namespaceDefinitions = namespaceDefinitions)
    if (!is.null(moduleSource$order)) {
        xmlAttrs(moduleSourceXML) <- c("order" = moduleSource$order)
    }
    vesselXML <- vesselToXML(moduleSource$vessel)
    xmlChildren(moduleSourceXML) <- list(vesselXML)
    return(moduleSourceXML)
}

## RUNNING A MODULE

#' Execute a \code{module}'s source(s)
#'
#' Execute the scripts contained in or referenced by a \code{module}'s sources.
#'
#' @details This function:
#' \itemize{
#'   \item creates a directory for the \code{module}'s outputs
#'   \item determines which language the module script requires
#'   \item executes the \code{module}'s source(s) using this language
#' }
#'
#' If the \code{module} has inputs the \code{inputObjects} list must
#' have a named absolute file location for each input which is not
#' resolveable based on only the input provided.
#'
#' \code{targetDirectory} must exist or the function will return an error.
#'
#' This function creates a directory named for the module in the
#' \code{targetDirectory} if it does not already exist. Outputs
#' generated by the module source scripts are stored in this
#' directory. A module XML file which will provide the original
#' module's output is also created in this directory.
#'
#' If \code{module$host} is not NULL the remote host must exist and be
#' accessible by conduit or this function will fail.
#'
#' @param module \code{module} object
#' @param targetDirectory File path for module output
#' @param inputObjects Named list of input objects
#' 
#' @seealso \code{module}, \code{moduleSource}
#'
#' @return a \code{moduleResult} object containing:
#' \item{file}{file path to resulting module XML}
#' \item{component}{resulting \code{module} object}
#' \item{outputList}{list of \code{output} objects produced by module}
#'
#' @examples
#'
#' ## run a module with no inputs
#' mod1xml <- system.file("extdata", "simpleGraph", "createGraph.xml", 
#' 		       package = "conduit")
#' mod1 <- loadModule("createGraph", 
#' 		      ref = mod1xml)
#' result1 <- runModule(module = mod1, targetDirectory = tempdir())
#' 
#' ## run a module with inputs
#' mod2xml <- system.file("extdata", "simpleGraph", "layoutGraph.xml",
#' 		          package = "conduit")
#' mod2 <- loadModule("layoutGraph", ref = mod2xml)
#' 
#' ## mod2 input names
#' names(mod2$inputs)
#' mod2inputs <- lapply(result1$outputList, getResult)
#' names(mod2inputs) <- names(mod2$inputs)
#' 
#' runModule(module = mod2, inputObjects = mod2inputs,
#'           targetDirectory = tempdir())
#' 
#' @export
runModule <- function(module, targetDirectory = getwd(),
                      inputObjects = NULL) {
    ## fail if not given a module
    if (class(module) != "module"){
        stop("'module' is not a 'module' object")
    }

    ## ensure targetDirectory exists
    targetDirectory <- file.path(targetDirectory)
    if (!file.exists(targetDirectory)) {
        stop("no such target directory")
    }

    name <- getName(module)
    moduleInputList <- module$inputs
    moduleOutputList <- module$outputs
    language <- getLanguage(module)
    host <- module$host

    ## create a directory for this module's output
    modulePath <- file.path(targetDirectory, getName(module))
    if (file.exists(modulePath))
        unlink(modulePath, recursive=TRUE)
    dir.create(modulePath, recursive=TRUE)

    ## enter output directory
    oldwd <- setwd(modulePath)
    on.exit(setwd(oldwd))

    ## if inputObjects is NULL, but module has inputs
    ## (we are running a module with inputs directly) 
    ## try to use inputs directly
    ## (e.g., external file or URL might work)
    if (is.null(inputObjects) && !is.null(moduleInputList)) {
        inputObjects <-
            lapply(moduleInputList,
                   function(x) resolveOutput(moduleOutputFromInput(x),
                                             language))
        names(inputObjects) <- names(moduleInputList)
    }
        
    ## prepare module inputs
    inputObjects <- lapply(X = moduleInputList, FUN = prepareInput,
                           inputList = inputObjects,
                           outputDirectory = modulePath,
                           language = language,
                           location = getLocation(module))

    script <- prepareScript(module)

    ## do host prep here
    host <- module$host
    hostSubdir <-
        if (!is.null(host)) {
            prepareModuleHost(host = host, name = name,
                              modulePath = modulePath)
        } else {
            NULL
        }
    
    ## execute script file
    exec_result <- executeScript(script, host, hostSubdir)
    if (exec_result != 0)
        stop("Unable to execute module script")

    ## get things back from host here
    if (!is.null(host)) {
        retrieveHost(host = host, hostSubdir = hostSubdir,
                     modulePath = modulePath)
    }


    ## resolve output objects
    outputList <- lapply(X = module$outputs, FUN = resolveOutput,
                         language = getLanguage(module),
                         outputDirectory = modulePath)

    ## return moduleResult object
    moduleResult(outputList, modulePath, module)
}

# Create a moduleOutput from a moduleInput
moduleOutputFromInput <- function(input) {
    moduleOutput(getName(input), getVessel(input), getFormat(input))
}

#' Prepare input object
#'
#' This function ensures a \code{module}'s \code{moduleInput}
#' requirements will be met when executed.
#'
#' For any \code{moduleInput} wrapping a \code{internalVessel} or
#' \code{fileVessel} with a relative ref, the relevant \code{input}
#' object is copied into the module \code{outputDirectory}.
#' 
#' @param moduleInput \code{moduleInput} object
#' @param inputList list of \code{input} objects provided to module
#' @param outputDirectory working directory for module execution
#' @param location location of originating module file
#'
#' @return \code{input} object
prepareInput <- function(moduleInput, inputList, outputDirectory,
                         language, location) {
    name <- getName(moduleInput)
    vessel <- getVessel(moduleInput)
    type <- getType(vessel)
    input <- getElement(inputList, name)

    input <- switch(
        type,
        internalVessel = {
            symbol <- vessel$symbol
            prepareInternalInput(input, symbol, language,
                                 outputDirectory)
        },
        fileVessel = prepareFileInput(vessel, input, outputDirectory,
                                      location),
        urlVessel = input,
        stop("unknown vessel type"))
    class(input) <- "input"
    input
}

prepareInternalInput <- function(input, symbol, language, outputDirectory) {
    internalInput <- file.path(
        outputDirectory, paste0(symbol, internalExtension(language)))
    if (!file.copy(input, internalInput))
        stop("unable to copy input into outputDirectory")
    if (file.exists(internalInput)) {
        return(internalInput)
    } else {
        stop("unable to prepare internalInput")
    }
}

prepareFileInput <- function(vessel, input, outputDirectory, location) {
    ref <- getRef(vessel)
    path <- vessel$path

    if (is.null(input)) {
        fileInput <- findFile(ref, path, location)
        if (is.null(fileInput))
            stop("unable to locate input file")
    } else if (is_absolute(ref)) {
        if (findFile(ref, path, location) != input)
            stop("input does not match path given in fileVessel")
        fileInput <- input
    } else {
        fileInput <- file.path(outputDirectory, ref)
        if (!file.copy(input, fileInput, overwrite = TRUE))
            stop("unable to copy input into outputDirectory")
    }
    fileInput
}

#' return \code{output} produced by a \code{moduleOutput}
#'
#' Return the object produced by a module output.
#'
#' This function returns a reference to the object produced by a
#' module's output when the module is executed. The reference
#' contained in this object is not guaranteed to exist until after
#' module execution. 
#'
#' @param moduleOutput \code{moduleOutput} object
#' @param language module script language
#' @param outputDirectory file location for module execution
#'
#' @return \code{output} list object, containing:
#'
#' \item{name}{output name}
#' \item{format}{\code{ioFormat} object}
#' \item{vessel}{\code{vessel} object}
#' \item{language}{module language}
#' \item{result}{address of output object produced}
#'
#' @export
output <- function(moduleOutput, language, outputDirectory) {
    if (!inherits(moduleOutput, "moduleOutput"))
        stop("moduleOutput object required")
    
    name <- getName(moduleOutput)
    format <- getFormat(moduleOutput)
    vessel <- getVessel(moduleOutput)
    type <- getType(vessel)

    ## calculate result
    result <-
        switch(type,
               internalVessel =
                   paste0(vessel$symbol, internalExtension(language)),
               urlVessel=,
               fileVessel = getRef(vessel),
               stop("vessel type not defined"))

    ## ensure absolute path
    if (type == "internalVessel" || type == "fileVessel") {
        result <- 
            if (!is_absolute(result)) {
                file.path(outputDirectory, result)
            } else if (file.exists(result)) {
                normalizePath(result)
            }
    }

    ## return output object
    output <-  list(name = name, format = format, vessel = vessel,
                    language = language, result = result)
    class(output) <- "output"
    output
}

#' Checks a module output object has been created.
#'
#' @details Will produce an error if the object does not exist.
#'
#' @param moduleOutput \code{moduleOutput} object
#' @param language module language
#' @param outputDirectory location of module output files
#'
#' @return \code{output} object
resolveOutput <- function (moduleOutput, language,
                           outputDirectory = getwd()) {
    name <- getName(moduleOutput)
    vessel <- getVessel(moduleOutput)
    type <- getType(vessel)
    output <- output(moduleOutput, language, outputDirectory)
    result <- getResult(output)

    if (type == "internalVessel" || type == "fileVessel") {
        if (!file.exists(result))
            stop(paste0("output object '", name, "' does not exist"))
    }
    return(output)
}

prepareModuleHost <- function (host, name, modulePath) {
    UseMethod("prepareModuleHost")
}

retrieveHost <- function(host, hostSubdir, modulePath) {
    UseMethod("retrieveHost")
}
