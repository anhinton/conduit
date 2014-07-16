\name{saveModule}
\alias{saveModule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Save a module to disk
  %%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
saveModule(module, targetDirectory = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{module}{
%%     ~~Describe \code{module} here~~
}
  \item{targetDirectory}{
%%     ~~Describe \code{targetDirectory} here~~
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
function (module, targetDirectory = getwd()) 
{
    require(XML)
    moduleDoc <- newXMLDoc(namespaces = "http://www.openapi.org/2014", 
        node = newXMLNode("module", attrs = c(name = module$name), 
            namespaceDefinitions = "http://www.openapi.org/2014/"))
    moduleRoot <- xmlRoot(moduleDoc)
    description <- newXMLNode("description", module$description)
    inputs <- lapply(module$inputs, function(i) {
        input <- newXMLNode("input", attrs = c(i["name"], i["type"]))
        input <- addChildren(input, newXMLNode("format", attrs = c(i["formatType"])))
    })
    outputs <- lapply(module$outputs, function(o) {
        attrs <- c(o["name"], o["type"])
        if (o["type"] == "external") {
            attrs <- c(attrs, o["ref"])
        }
        output <- newXMLNode("output", attrs = attrs)
        output <- addChildren(output, newXMLNode("format", attrs = c(o["formatType"])))
    })
    platform <- newXMLNode("platform", attrs = c(module$platform))
    sources <- lapply(module$sources, function(s) {
        newXMLNode(name = "source", attrs = c(s["type"], s["order"]), 
            newXMLCDataNode(s$value))
    })
    moduleRoot <- addChildren(moduleRoot, kids = list(description, 
        platform, inputs, sources, outputs))
    moduleFilePath <- file.path(targetDirectory, paste(module$name, 
        ".xml", sep = ""))
    saveXML(moduleDoc, moduleFilePath)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
