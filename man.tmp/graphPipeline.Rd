\name{graphPipeline}
\alias{graphPipeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
graphPipeline(pipeline)
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
    require(graph)
    require(RBGL)
    moduleNames <- names(pipeline$modules)
    pipes.list <- lapply(pipeline$pipes, function(x) {
        startModule <- x$start["module"]
        startOutput <- x$start["name"]
        endModule <- x$end["module"]
        endInput <- x$end["name"]
        pipe <- c(startModule, startOutput, endModule, endInput)
        names(pipe) <- c("startModule", "startOutput", "endModule", 
            "endInput")
        pipe
    })
    pipes.matrix <- do.call(rbind, pipes.list)
    edgeList <- lapply(moduleNames, function(startModule, pipes) {
        isStartModule <- pipes[, 1] == startModule
        list(edges = pipes[isStartModule, 3])
    }, pipes.matrix)
    names(edgeList) <- moduleNames
    new("graphNEL", nodes = moduleNames, edgeL = edgeList, edgemode = "directed")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
