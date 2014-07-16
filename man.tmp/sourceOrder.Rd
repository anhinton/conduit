\name{sourceOrder}
\alias{sourceOrder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sourceOrder(sources)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sources}{
%%     ~~Describe \code{sources} here~~
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
function (sources) 
{
    orderValues <- sapply(sources, function(x) {
        as.numeric(x$order)
    })
    zeroLess <- !is.na(orderValues) & orderValues <= 0
    zeroLessOrder <- order(orderValues[zeroLess])
    zeroLessOrdered <- which(zeroLess)[zeroLessOrder]
    pos <- !is.na(orderValues) & orderValues > 0
    posOrder <- order(orderValues[pos])
    posOrdered <- which(pos)[posOrder]
    unorderedOrdered <- which(is.na(orderValues))
    c(zeroLessOrdered, unorderedOrdered, posOrdered)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
