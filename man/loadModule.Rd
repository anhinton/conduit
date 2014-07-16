\name{loadModule}
\alias{loadModule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Load a module from disk
  %%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loadModule(filename, namespaces = c(oa = "http://www.openapi.org/2014/"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{namespaces}{
%%     ~~Describe \code{namespaces} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filename, namespaces = c(oa = "http://www.openapi.org/2014/")) 
{
    require(tools)
    require(XML)
    require(RCurl)
    module <- xmlRoot(xmlParse(filename))
    name <- file_path_sans_ext(basename(filename))
    descNodes <- getNodeSet(module, "//description|//oa:description", 
        namespaces = namespaces)
    description <- if (length(descNodes)) {
        xmlValue(descNodes[[1]])
    }
    else {
        character(1)
    }
    inputNodes <- getNodeSet(module, "//input|//oa:input", namespaces = namespaces)
    inputs <- if (length(inputNodes) == 0) {
        NULL
    }
    else {
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
                formatType <- if (any(names(formAttrs) == "formatType")) {
                  formAttrs[["formatType"]]
                }
                else {
                  "text"
                }
                list(format = xmlValue(formatNode), type = formatType)
            }
            else {
                list(format = "", type = "text")
            }
            c(name = name, type = type, format = format$format, 
                formatType = format$type)
        })
        names(inputs) <- inputNames
        inputs
    }
    platformNode <- getNodeSet(module, "//platform|//oa:platform", 
        namespaces = namespaces)[[1]]
    platform <- xmlAttrs(platformNode)[["name"]]
    names(platform) <- "name"
    sourceNodes <- getNodeSet(module, "//source|//oa:source", 
        namespaces = namespaces)
    sources <- lapply(sourceNodes, function(node) {
        attrs <- xmlAttrs(node)
        type <- if (length(attrs) && names(attrs) == "type") {
            attrs[["type"]]
        }
        else {
            character(1)
        }
        order <- if (length(attrs) && any(grepl("order", names(attrs)))) {
            attrs[["order"]]
        }
        else {
            character(1)
        }
        value <- if (length(attrs) && any(grepl("ref", names(attrs)))) {
            ref <- attrs[["ref"]]
            value <- if (grepl("^ *https://", ref)) {
                getURL(ref)
            }
            else {
                readLines(ref)
            }
        }
        else {
            xmlValue(node)
        }
        list(value = value, type = type, order = order)
    })
    sources <- lapply(sourceOrder(sources), function(x, sources) {
        sources[[x]]
    }, sources)
    outputNodes <- getNodeSet(module, "//output|//oa:output", 
        namespaces = namespaces)
    outputs <- if (length(outputNodes) == 0) {
        NULL
    }
    else {
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
                formatType <- if (any(names(formAttrs) == "formatType")) {
                  formAttrs[["formatType"]]
                }
                else {
                  "text"
                }
                list(format = xmlValue(formatNode), type = formatType)
            }
            else {
                list(format = "", type = "text")
            }
            ref <- if (type == "external") {
                attrs[["ref"]]
            }
            else {
                character(1)
            }
            c(name = name, type = type, format = format$format, 
                formatType = format$type, ref = ref)
        })
        names(outputs) <- outputNames
        outputs
    }
    list(name = name, description = description, inputs = inputs, 
        platform = platform, sources = sources, outputs = outputs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
