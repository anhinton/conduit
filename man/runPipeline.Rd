\name{runPipeline}
\alias{runPipeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Run a pipeline
  %%  ~~function to do ... ~~
}
\description{
  Attempt to run an OpenAPI pipeline.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runPipeline(pipeline)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pipeline}{
    Pipeline list object
%%     ~~Describe \code{pipeline} here~~
}
}
\details{
  This function will attempt to run an OpenAPI pipeline list object,
  either loaded from a pipeline .XML file with \code{loadPipeline} or
  created with \code{pipeline} [\code{pipeline} does not exist as at
  2014-07-17]].

  First the function will determine the order in which its modules are
  to be run. Note that the pipeline is not allowed to have any cycles or
  the function will fail.

  The function then determines the file paths of the objects described
  in the pipes as inputs/outputs.

  Finally the function runs each module in the order determined,
  feeding each module its inputs.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Meaningless list object. TODO: fix what runPipeline, runModule,
  runPlatform return.
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
