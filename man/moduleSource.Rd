\name{moduleSource}
\alias{moduleSource}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a source for a module
  %%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
moduleSource(valueRef, type = "", order = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{valueRef}{
%%     ~~Describe \code{valueRef} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{order}{
%%     ~~Describe \code{order} here~~
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
function (valueRef, type = "", order = "") 
{
    value <- readLines(valueRef)
    list(value = value, type = type, order = order)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
