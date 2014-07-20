\name{moduleSource}
\alias{moduleSource}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a source for a module
  %%  ~~function to do ... ~~
}
\description{
  Create an OpenAPI module source from an external text document
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
moduleSource(value, ref=NULL, type = "", order = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value}{
    Source script as character vector
  }
  \item{ref}{
    File path of document to be wrapped as module
    %%     ~~Describe \code{valueRef} here~~
  }
  \item{type}{
    Character value (2014-07-21: not used)
    %%     ~~Describe \code{type} here~~
  }
  \item{order}{
    Numeric value
    %%     ~~Describe \code{order} here~~
  }
}
\details{
  If ref is provided the source is created using the contents of the
  text file at ref. Otherwise the source is created using the character
  vector value.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \code{moduleSource} list
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
