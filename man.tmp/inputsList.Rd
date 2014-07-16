\name{inputsList}
\alias{inputsList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inputsList(pipes, modules, pipelinePath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pipes}{
%%     ~~Describe \code{pipes} here~~
}
  \item{modules}{
%%     ~~Describe \code{modules} here~~
}
  \item{pipelinePath}{
%%     ~~Describe \code{pipelinePath} here~~
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
function (pipes, modules, pipelinePath) 
{
    inputNames <- lapply(pipes, function(x) {
        x$end["name"]
    })
    inputsList <- lapply(pipes, function(x, modules, pipelinePath) {
        endModule <- modules[[x$end["module"]]]
        platform <- endModule$platform["name"]
        type <- endModule$inputs[[x$end["name"]]]["type"]
        if (type == "internal") {
            input <- file.path(pipelinePath, "modules", x$start["module"], 
                paste(x$start["name"], internalExtension(platform), 
                  sep = ""))
        }
        else if (type == "external") {
            startModule <- modules[[x$start["module"]]]
            input <- startModule$outputs[[x$start["name"]]]["ref"]
            if (dirname(input) == ".") {
                input <- file.path(pipelinePath, "modules", x$start["module"], 
                  input)
            }
        }
        input
    }, modules, pipelinePath)
    names(inputsList) <- inputNames
    inputsList
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
