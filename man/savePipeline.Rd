\name{savePipeline}
\alias{savePipeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Save a pipeline to file
}
\description{
  Saves a pipeline to disk as an OpenAPI pipeline .XML file in the
  specified directory.
}
\usage{
savePipeline(pipeline, targetDirectory = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pipeline}{
    A pipeline list object.
}
  \item{targetDirectory}{
    Target to directory in which to save pipeline.xm;
}
}
\details{
  This function will save a pipeline list object created by \code{pipeline}
  or \code{loadPipeline} to disk as an OpenAPI pipeline.xml file. The
  resulting file is always called sQuote{pipeline.xml}.

  
}
\value{
  File path of resulting file as character value.
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
  Ashley Noel Hinton
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
function (pipeline, targetDirectory = getwd()) 
{
    require(XML)
    pipelineDoc <- newXMLDoc(namespaces = "http://www.openapi.org/2014", 
        node = newXMLNode("pipeline", attrs = c(name = pipeline$name), 
            namespaceDefinitions = "http://www.openapi.org/2014/"))
    pipelineRoot <- xmlRoot(pipelineDoc)
    description <- newXMLNode("description", pipeline$description)
    modules <- lapply(pipeline$modules, function(m) {
        newXMLNode("module", attrs = c(name = m$name))
    })
    names(modules) <- NULL
    pipes <- lapply(pipeline$pipes, function(p) {
        startAttrs <- c(p$start["module"], p$start["name"])
        endAttrs <- c(p$end["module"], p$end["name"])
        pipe <- newXMLNode("pipe")
        pipe <- addChildren(pipe, kids = list(newXMLNode("start", 
            attrs = startAttrs), newXMLNode("end", attrs = endAttrs)))
    })
    pipelineRoot <- addChildren(pipelineRoot, kids = list(description, 
        modules, pipes))
    pipelineFilePath <- file.path(targetDirectory, "pipeline.xml")
    saveXML(pipelineDoc, pipelineFilePath)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
