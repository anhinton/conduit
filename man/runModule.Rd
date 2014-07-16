\name{runModule}
\alias{runModule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Run a module
  %%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runModule(module, inputs = list(), targetDirectory = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{module}{
%%     ~~Describe \code{module} here~~
}
  \item{inputs}{
%%     ~~Describe \code{inputs} here~~
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
function (module, inputs = list(), targetDirectory = getwd()) 
{
    require(tools)
    modulePath <- file.path(targetDirectory, "modules", module$name)
    if (file.exists(modulePath)) 
        unlink(modulePath, recursive = TRUE)
    dir.create(modulePath, recursive = TRUE)
    moduleFiles <- file_path_as_absolute(modulePath)
    modulePlatform <- module$platform
    class(module) <- modulePlatform
    runPlatform(module, inputs, modulePath)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
