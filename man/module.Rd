\name{module}
\alias{module}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a module
  %%  ~~function to do ... ~~
}
\description{
  Create an OpenAPI \code{module} list.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
module(name, description = "", platform, inputs = list(), outputs = list(), sources = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
    Name of module as character value
    %%     ~~Describe \code{name} here~~
  }
  \item{description}{
    Description of what module does as character value
    %%     ~~Describe \code{description} here~~
  }
  \item{platform}{
    Module platform as character value
    %%     ~~Describe \code{platform} here~~
  }
  \item{inputs}{
    List of \code{moduleInput} objects
    %%     ~~Describe \code{inputs} here~~
  }
  \item{outputs}{
    List of \code{moduleOutput} objects
    %%     ~~Describe \code{outputs} here~~
  }
  \item{sources}{
    List of \code{moduleSource} objects
    %%     ~~Describe \code{sources} here~~
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  OpenAPI \code{module} list
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
