\name{runPipeline}
\alias{runPipeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runPipeline(pipeline)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pipeline}{
%%     ~~Describe \code{pipeline} here~~
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
function (pipeline) 
{
    require(tools)
    if (!file.exists("pipelines")) 
        dir.create("pipelines")
    pipelineName <- pipeline$name
    pipelinePath <- file.path("pipelines", pipelineName)
    if (file.exists(pipelinePath)) 
        unlink(pipelinePath, recursive = TRUE)
    dir.create(pipelinePath, recursive = TRUE)
    pipelinePath <- file_path_as_absolute(pipelinePath)
    modules <- pipeline$modules
    moduleNames <- names(modules)
    moduleGraph <- graphPipeline(pipeline)
    moduleOrder <- tsort(moduleGraph)
    inputs <- inputsList(pipeline$pipes, modules, pipelinePath)
    x <- lapply(moduleOrder, function(x, modules, inputs, pipelinePath) {
        runModule(modules[[x]], inputs, pipelinePath)
    }, modules, inputs, pipelinePath)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
