\name{pipeline}
\alias{pipeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a pipeline
  %%  ~~function to do ... ~~
}
\description{
  Create an OpenAPI pipeline list
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pipeline(name, description = "", modules = list(), pipes = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
    Pipeline name as character value
    %%     ~~Describe \code{name} here~~
  }
  \item{description}{
    Pipeline description as character value
    %%     ~~Describe \code{description} here~~
  }
  \item{modules}{
    List of modules created with \code{module} or \code{loadModule}
    %%     ~~Describe \code{modules} here~~
  }
  \item{pipes}{
    List of pipes created with \code{pipe}
    %%     ~~Describe \code{pipes} here~~
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Pipeline list
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
