\name{loadPipeline}
\alias{loadPipeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Load a pipeline and its modules from disk
  %%  ~~function to do ... ~~
}
\description{
  Read an OpenAPI pipeline and its associated modules from an XML document
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loadPipeline(filename, pipelineName = file_path_sans_ext(basename(filename)), namespaces = c(oa = "http://www.openapi.org/2014/"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
    \code{pipeline} XML document
    %%     ~~Describe \code{filename} here~~
  }
  \item{pipelineName}{
    character value giving name of pipeline (defaults to name of
    pipeline file)
    %%     ~~Describe \code{pipelineName} here~~
  }
  \item{namespaces}{
    Extra namespaces used in XML given as named character vector
    %%     ~~Describe \code{namespaces} here~~
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \code{pipeline} list
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
