\name{loadPipeline}
\alias{loadPipeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Load a pipeline and its modules from disk
  %%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loadPipeline(filename, pipelineName = file_path_sans_ext(basename(filename)), namespaces = c(oa = "http://www.openapi.org/2014/"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{pipelineName}{
%%     ~~Describe \code{pipelineName} here~~
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
function (filename, pipelineName = file_path_sans_ext(basename(filename)), 
    namespaces = c(oa = "http://www.openapi.org/2014/")) 
{
    require(tools)
    require(XML)
    filename <- file_path_as_absolute(filename)
    pipelineDir <- dirname(filename)
    pipeline <- xmlRoot(xmlParse(filename))
    descNodes <- getNodeSet(pipeline, "//description|//oa:description", 
        namespaces = namespaces)
    description <- if (length(descNodes)) {
        xmlValue(descNodes[[1]])
    }
    else {
        character(1)
    }
    moduleNodes <- getNodeSet(pipeline, "//module|//oa:module", 
        namespaces = namespaces)
    pipeNodes <- getNodeSet(pipeline, "//pipe|//oa:pipe", namespaces = namespaces)
    moduleFilePaths <- sapply(moduleNodes, function(x, pipelineDir) {
        attrs <- xmlAttrs(x)
        if (any(names(attrs) == "name")) {
            filename <- paste0(attrs[["name"]], ".xml")
            file.path(pipelineDir, filename)
        }
        else if (any(names(attrs) == "ref")) {
            attrs[["ref"]]
        }
    }, pipelineDir)
    modules <- lapply(moduleFilePaths, loadModule, namespaces)
    moduleNames <- sapply(modules, function(x) {
        x$name
    })
    names(modules) <- moduleNames
    pipes <- lapply(pipeNodes, function(x, namespaces) {
        start <- xmlChildren(x)$start
        startModule <- xmlAttrs(start)[["module"]]
        startName <- xmlAttrs(start)[["name"]]
        start <- c(module = startModule, name = startName)
        end <- xmlChildren(x)$end
        endModule <- xmlAttrs(end)[["module"]]
        endName <- xmlAttrs(end)[["name"]]
        end <- c(module = endModule, name = endName)
        list(start = start, end = end)
    }, namespaces)
    list(name = pipelineName, description = description, modules = modules, 
        pipes = pipes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
