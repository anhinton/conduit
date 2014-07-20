\name{runModule}
\alias{runModule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Run a module
  %%  ~~function to do ... ~~
}
\description{
  Run an OpenAPI module which has been created with \code{loadModule} of
  \code{module}
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runModule(module, inputs = list(), targetDirectory = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{module}{
    OpenAPI module list
    %%     ~~Describe \code{module} here~~
  }
  \item{inputs}{
    Named list of file paths corresponding to inputs
    %%     ~~Describe \code{inputs} here~~
  }
  \item{targetDirectory}{
    File path where module is to be run
    %%     ~~Describe \code{targetDirectory} here~~
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Empty list because helper functions don't do sensible returns just yet
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
