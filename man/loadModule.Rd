\name{loadModule}
\alias{loadModule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Load a module from disk
  %%  ~~function to do ... ~~
}
\description{
  Read an OpenAPI module from an XML file
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loadModule(filename, namespaces = c(oa = "http://www.openapi.org/2014/"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
    File path of module XML document
    %%     ~~Describe \code{filename} here~~
  }
  \item{namespaces}{
    Namespaces used in XML document as named character vector
    %%     ~~Describe \code{namespaces} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \code{module} list
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
