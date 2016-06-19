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
#' @param language \code{moduleLanguage} object describing source
#'     script execution language
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
#' \item{language}{\code{moduleLanguage} object}
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
#' mod1 <- module(name = "setX", language = moduleLanguage("R"),
#'                description = "sets the value of x",
#'                outputs = list(outp1),
#'                sources = list(src1))
#' 
#' ## create a module with one input and one source
#' mod2 <-
#'     module(
#'         "showY",
#'         language = moduleLanguage("R"),
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
        if (!inherits(language, "moduleLanguage"))
            stop("'language' is not a moduleLanguage object")
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

#' Create a \code{moduleLanguage} object
#'
#' @param language Language name
#' @param minVersion Minimum version required
#' @param maxVersion Maximum version required
#' @param version Exact version required
#'
#' @return \code{moduleLanguage} object
#'
#' @export
moduleLanguage <- function(language, minVersion = NULL, maxVersion = NULL,
                           version = NULL) {
    if (!is.null(version))
        minVersion = maxVersion = NULL
    language = execLanguage(language = language, minVersion = minVersion,
                            maxVersion = maxVersion, version = version)
    moduleLanguage <- list(language = language, minVersion = minVersion,
                           maxVersion = maxVersion, version = version)
    class(moduleLanguage) <- c(
        paste0(moduleLanguage$language, "ModuleLanguage"),
        "moduleLanguage")
    moduleLanguage
}

#' @describeIn getLanguage
#'
#' Return language name as character
#'
#' @export
getLanguage.moduleLanguage <- function(x) {
    x$language
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
#' Returns \code{moduleLanguage} object
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
#' @param ref Module location or filename or a \code{vessel} object
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
    ## TODO(anhinton): change how modules are loaded to include
    ## loading from URLs, files etc. The following code uses vessel
    ## objects to provide a temporary solution
    if (!inherits(ref, "vessel"))
        ref <- fileVessel(ref, path)
    ## fetch module XML from disk
    rawXML <- tryCatch(
        fetchVessel(ref),
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

#' Create a \code{moduleLanguage} object from language XML
#'
#' @param moduleLanguageXML language XML node
#'
#' @return \code{moduleLanguage} object
#'
#' @import XML
readModuleLanguageXML <- function(moduleLanguageXML) {
    value <- xmlValue(moduleLanguageXML)
    nodeAttrs <- xmlAttrs(moduleLanguageXML)
    minVersion <- getXMLAttr(moduleLanguageXML, "minVersion")
    maxVersion <- getXMLAttr(moduleLanguageXML, "maxVersion")
    version <- getXMLAttr(moduleLanguageXML, "version")
    moduleLanguage(language = value, minVersion = minVersion,
                   maxVersion = maxVersion, version = version)
}

#' Create a \code{moduleHost} object from host XML
#'
#' This function creates a \code{moduleHost} object from valid host
#' elements.
#'
#' As of 2016-05-13 \samp{<docker/>}, \samp{<moduleInput/>} and
#' \samp{<vagrant/>} elements are supported.
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
        docker = readDockerHostXML(moduleHostXML),
        moduleInput = readModuleInputHostXML(moduleHostXML),
        vagrant = readVagrantHostXML(moduleHostXML)
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
#' @details If <host/> contains <moduleInput/> function fails if
#'     moduleInput name does not match any input names.
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

    ## extract language
    languageNode <- nodes$language
    language <- readModuleLanguageXML(languageNode)

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

    ## check that any moduleInput host matches a named input
    if (inherits(host, "moduleInputHost")) {
        if (!(host$name) %in% sapply(inputs, getName))
            stop("moduleInput host name does not match any input names")
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
#' 
#' @import XML
#' @export
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
                             namespaceDefinitions = namespaceDefinitions)
    language <- moduleLanguageToXML(getLanguage(module))
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
                kids=list(language, host, description, inputs, sources,
                          outputs))
}

#' Create XML corresponding to \code{moduleLanguage} object
#'
#' @param moduleLanguage \code{moduleLanguage} objects
#'
#' @return XML node representing module language
moduleLanguageToXML <- function(moduleLanguage) {
    if (!inherits(moduleLanguage, "moduleLanguage"))
        stop("moduleLanguage object required")
    language <- getLanguage(moduleLanguage)
    minVersion <- moduleLanguage$minVersion
    maxVersion <- moduleLanguage$maxVersion
    version <- moduleLanguage$version
    moduleLanguageXML <-
        newXMLNode(name = "language",
                   language,
                   attrs = c(minVersion = minVersion,
                             maxVersion = maxVersion,
                             version = version))
    moduleLanguageXML
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
#' Execute the scripts contained in or referenced by a \code{module}'s
#' sources.
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
#' If \code{warnVersion} is \code{TRUE} this function will give a
#' warning when the executing language does not meet the module's
#' \code{moduleLanguage} requirments.
#'
#' @param module \code{module} object
#' @param targetDirectory File path for module output
#' @param inputObjects Named list of input objects
#' @param warnVersion should conduit warn if module language
#'     version requirements are not met
#' 
#' @seealso \code{module}, \code{moduleSource}, \code{moduleLanguage}
#'     for creating modules, \code{moduleResult} for result objects
#'
#' @return \code{moduleResult}
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
#' mod2inputs <- lapply(result1$outputList, getRef)
#' names(mod2inputs) <- names(mod2$inputs)
#' 
#' runModule(module = mod2, inputObjects = mod2inputs,
#'           targetDirectory = tempdir())
#'
#' @import XML
#' @export
runModule <- function(module, targetDirectory = getwd(),
                      inputObjects = NULL, warnVersion = FALSE) {
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
    moduleLanguage <- getLanguage(module)
    
    ## create a directory for this module's output
    modulePath <- file.path(targetDirectory, getName(module))
    if (file.exists(modulePath))
        unlink(modulePath, recursive=TRUE)
    dir.create(modulePath, recursive=TRUE)

    ## enter output directory
    oldwd <- setwd(modulePath)
    on.exit(setwd(oldwd))

    ## prepare module inputs
    inputObjects <- lapply(X = moduleInputList, FUN = prepareInput,
                           inputList = inputObjects,
                           outputDirectory = modulePath,
                           moduleLanguage = moduleLanguage,
                           location = getLocation(module))

    ## prepare script
    script <- prepareScript(module)

    ## if moduleHost is waiting for a module input, load this now
    moduleHost <- module$host
    if (inherits(moduleHost, "moduleInputHost")) {
        ## load moduleHost from named input
        inputName <- getName(moduleHost)
        inputType <- getType(getVessel(moduleInputList[[inputName]]))
        rawXML <- switch(
            inputType,
            fileVessel =,
            urlVessel = readLines(inputObjects[[inputName]]),
            stop(paste("unable to load a moduleHost of type", inputType,
                       "from input")))
        xml <- xmlRoot(xmlParse(rawXML))
        moduleHost <- readModuleHostXML(xml)
        ## don't let moduleInputHost objects recurse
        if (inherits(moduleHost, "moduleInputHost"))
            stop("moduleInputHost points to another moduleInputHost!")
    }

    ## prepare moduleHost
    outputLocation <-
        if (!is.null(moduleHost)) {
            prepareModuleHost(moduleHost = moduleHost, moduleName = name,
                              modulePath = modulePath)
        } else {
            NULL
        }
    
    ## execute script file
    exec_result <- executeScript(script, moduleHost, outputLocation)
    if (!is.null(attr(exec_result, "status"))) {
        stop(c("Unable to execute module script: ", exec_result))
    }
        
    ## retrieve outputs from moduleHost
    if (!is.null(moduleHost)) {
        retrieveModuleHost(moduleHost = moduleHost,
                           outputLocation = outputLocation,
                           modulePath = modulePath)
    }

    ## resolve output objects
    outputList <- lapply(X = module$outputs, FUN = resolveOutput,
                         moduleLanguage = moduleLanguage,
                         outputDirectory = modulePath)

    ## create moduleResult object
    moduleResult <- moduleResult(outputList, modulePath, module)

    ## warn if language versions not met
    if (warnVersion) 
        warnLanguageVersion(module = module, moduleResult = moduleResult)

    moduleResult
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
#' For any \code{moduleInput} that is not found in \code{inputList}
#' (e.g., when the module is being run directly), the \code{moduleInput}
#' itself is tried (e.g., a URL vessel should work).
#' 
#' @param moduleInput \code{moduleInput} object
#' @param inputList list of \code{input} objects provided to module
#' @param outputDirectory working directory for module execution
#' @param moduleLanguage \code{moduleLanguage} object
#' @param location location of originating module file
#'
#' @return \code{input} object. Generally a character string
#'     referencing a file location or URL
prepareInput <- function(moduleInput, inputList, outputDirectory,
                         moduleLanguage, location) {
    name <- getName(moduleInput)
    vessel <- getVessel(moduleInput)
    type <- getType(vessel)
    input <- getElement(inputList, name)

    input <- switch(
        type,
        internalVessel = {
            symbol <- vessel$symbol
            prepareInternalInput(input, symbol, moduleLanguage,
                                 outputDirectory)
        },
        fileVessel = prepareFileInput(vessel, input, outputDirectory,
                                      location),
        urlVessel = prepareURLInput(vessel, input),
        stop("unknown vessel type"))
    class(input) <- "input"
    input
}

#' Prepare internal input object.
#'
#' This function makes a serialized internal module input available in
#' a module's working directory.
#'
#' @param input File path to serialized object
#' @param symbol Name of module input
#' @param moduleLanguage \code{moduleLanguage} object
#' @param outputDirectory File path to module working directory
#'
#' @return File path to serialized internal input.
prepareInternalInput <- function(input, symbol, moduleLanguage,
                                 outputDirectory) {
    internalInput <- file.path(
        outputDirectory, paste0(symbol, internalExtension(moduleLanguage)))
    if (!file.copy(input, internalInput))
        stop("unable to copy input into outputDirectory")
    if (file.exists(internalInput)) {
        return(internalInput)
    } else {
        stop("unable to prepare internalInput")
    }
}

#' Prepare file input object
#'
#' This function makes sure a module's file input is available to the
#' module.
#'
#' If \code{input} is NULL the module is assumed to be
#' \dQuote{starting} from a file, and the file referenced in
#' \code{vessel} is returned.
#'
#' Where \code{input} is not null, and \code{vessel} describes an
#' absolute path to a file this path is returned. If \code{vessel}
#' describes a relative path to a file, this file is copied into the
#' module's outputDirectory, and the resulting file is returned.
#'
#' @param vessel Module input vessel object
#' @param input File path to input
#' @param outputDirectory File path to module working directory
#' @param location File path to originating module XML file
#'
#' @return File path to input file
prepareFileInput <- function(vessel, input, outputDirectory, location) {
    ref <- getRef(vessel)
    path <- vessel$path
    
    if (is.null(input)) {
        input <- findFile(ref, path, location)
        if (is.null(input))
            stop("unable to locate input file")
        if (is_absolute(ref)) {
            fileInput <- input
        } else {
            fileInput <- file.path(outputDirectory, ref)
            if (!file.copy(input, fileInput, overwrite = TRUE))
                stop("unable to copy input into outputDirectory")
        }
    } else {
        if (is_absolute(ref)) {
            if (findFile(ref, path, location) != input)
                stop("input does not match path given in fileVessel")
            fileInput <- input
        } else {
            fileInput <- file.path(outputDirectory, ref)
            if (!file.copy(input, fileInput, overwrite = TRUE))
                stop("unable to copy input into outputDirectory")
        }
    }
    fileInput
}

#' Prepare URL input object
#'
#' This function ensure's a module's URL input is available to the
#' module.
#'
#' If \code{input} is NULL the module is assumed to be
#' \dQuote{starting} from a URL, and the URL referenced in
#' \code{vessel} is returned.
#'
#' @param vessel Module input vessel object
#' @param input URL string
#'
#' @return URL to input resource
prepareURLInput <- function(vessel, input) {
    ref <- getRef(vessel)
    ## Allow for module being run in isolation (inputs are NULL)
    if (is.null(input)) {
        ref
    } else {
        if (ref != input)
            stop("input does not match URL given in urlVessel")
        input
    }
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
#' @param moduleLanguage \code{moduleLanguage} object
#' @param outputDirectory file location for module execution
#'
#' @return \code{output} list object, containing:
#'
#' \item{name}{output name}
#' \item{format}{\code{ioFormat} object}
#' \item{vessel}{\code{vessel} object}
#' \item{moduleLanguage}{\code{moduleLanguage} object}
#' \item{ref}{address of output object produced}
#'
#' @export
output <- function(moduleOutput, moduleLanguage, outputDirectory) {
    if (!inherits(moduleOutput, "moduleOutput"))
        stop("moduleOutput object required")
    
    name <- getName(moduleOutput)
    format <- getFormat(moduleOutput)
    vessel <- getVessel(moduleOutput)
    type <- getType(vessel)

    ## calculate ref
    ref <-
        switch(type,
               internalVessel =
                   paste0(vessel$symbol,
                          internalExtension(moduleLanguage)),
               urlVessel=,
               fileVessel = getRef(vessel),
               stop("vessel type not defined"))

    ## ensure absolute path
    if (type == "internalVessel" || type == "fileVessel") {
        ref <- 
            if (!is_absolute(ref)) {
                file.path(outputDirectory, ref)
            } else if (file.exists(ref)) {
                normalizePath(ref)
            }
    }

    ## return output object
    output <-  list(name = name, format = format, vessel = vessel,
                    moduleLanguage = moduleLanguage, ref = ref)
    class(output) <- "output"
    output
}

#' @describeIn getLanguage
#'
#' Get \code{moduleLanguage} object
#'
#' @export
getLanguage.output <- function(x) {
    x$moduleLanguage
}

#' Checks a module output object has been created.
#'
#' @details Will produce an error if the object does not exist.
#'
#' @param moduleOutput \code{moduleOutput} object
#' @param moduleLanguage \code{moduleLanguage} object
#' @param outputDirectory location of module output files
#'
#' @return \code{output} object
resolveOutput <- function (moduleOutput, moduleLanguage,
                           outputDirectory = getwd()) {
    name <- getName(moduleOutput)
    vessel <- getVessel(moduleOutput)
    type <- getType(vessel)
    output <- output(moduleOutput, moduleLanguage, outputDirectory)
    ref <- getRef(output)

    ## TODO(anhinton): write check for URL outputs
    if (type == "internalVessel" || type == "fileVessel") {
        if (!file.exists(ref))
            stop(paste0("output object '", name, "' does not exist"))
    }
    return(output)
}

#' Prepare a module host for execution.
#'
#' These methods ensure that a \code{moduleHost} will have all
#' resources required to execute a \code{module}'s source scripts.
#'
#' These methods return the path to the module output directory as an
#' \code{outputLocation} object.
#'
#' @param moduleHost \code{moduleHost} object
#' @param moduleName module name
#' @param modulePath module output directory
#'
#' @return \code{outputLocation} object
prepareModuleHost <- function (moduleHost, moduleName, modulePath) {
    if (!inherits(moduleHost, "moduleHost"))
        stop("moduleHost object required")
    if (!is_length1_char(moduleName))
        stop("moduleName is not length 1 character")
    if (!dir.exists(modulePath))
        stop("modulePath does not exist")
    UseMethod("prepareModuleHost")
}

#' Retrieve results of running a module's source scripts on a
#' \code{moduleHost}
#'
#' This functions retrieves the results of executing a module's source
#' scripts on a \code{moduleHost} using \code{executeScript}.
#'
#' \code{outputLocation} should be the result of
#' \code{prepareModuleHost} on the same \code{moduleHost}.
#'
#' @param moduleHost \code{moduleHost} object
#' @param outputLocation \code{outputLocation} object
#' @param modulePath output directory on local machine
#'
#' @return NULL if successful
#'
#' @seealso \code{moduleHost}, \code{prepareModuleHost},
#'     \code{executeScript}
retrieveModuleHost <- function(moduleHost, outputLocation, modulePath) {
    if (!inherits(moduleHost, "moduleHost"))
        stop("moduleHost object required")
    if (!inherits(outputLocation, "outputLocation") && !is.null(outputLocation))
        stop("outputLocation object required")
    if (!dir.exists(modulePath))
        stop("modulePath does not exist")
    UseMethod("retrieveModuleHost")
}

#' Give warning if module execution violated moduleLanguage versions.
#'
#' If the version of the language used to execute a module's source
#' scripts has violated the \code{moduleLanguage}'s minVersion,
#' maxVersion or version, this function will raise a warning.
#'
#' @param module \code{module} object
#' @param moduleResult \code{moduleResult} object
#'
#' @seealso \code{runModule}
#' 
#' @return NULL
warnLanguageVersion <- function(module, moduleResult) {
    if (!inherits(module, "module"))
        stop("module required")
    if (!inherits(moduleResult, "moduleResult"))
        stop("moduleResult required")

    ## execution language info
    execLanguageVersion <- moduleResult$execLanguageVersion
    execVersion <- execLanguageVersion$execVersion

    ## module language requirements
    moduleLanguage <- getLanguage(module)
    moduleName <- getName(module)

    ## warnings
    if (execLanguageVersion$failMin) {
        warning(paste(getLanguage(moduleLanguage),
                      execVersion, "was less than minVersion",
                      moduleLanguage$minVersion,
                      "when executing module", moduleName))
    }
    if (execLanguageVersion$failMax) {
        warning(paste(getLanguage(moduleLanguage),
                      execVersion, "was greater than maxVersion",
                      moduleLanguage$maxVersion,
                      "when executing module", moduleName))
    }
    if (execLanguageVersion$failExact) {
        warning(paste(getLanguage(moduleLanguage),
                      execVersion, "was not exactly version",
                      moduleLanguage$version,
                      "when executing module", moduleName))
    }
}
