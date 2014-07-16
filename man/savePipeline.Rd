\name{savePipeline}
\alias{savePipeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Save a pipeline to file
}
\description{
  Saves a pipeline to disk as an OpenAPI pipeline .XML file in the
  specified directory.
}
\usage{
savePipeline(pipeline, targetDirectory = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pipeline}{
    A pipeline list object.
}
  \item{targetDirectory}{
    Directory in which to save pipeline.xm;
}
}
\details{
  This function will save a pipeline list object created by \code{pipeline}
  or \code{loadPipeline} to disk as an OpenAPI pipeline.xml file. The
  resulting file is always called sQuote{pipeline.xml}.  
}
\value{
  File path of resulting file as character value.
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
\keyword{ File }% __ONLY ONE__ keyword per line
