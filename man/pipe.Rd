\name{pipe}
\alias{pipe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a pipe
%%  ~~function to do ... ~~
}
\description{
  Create a pipe for use in an OpenAPI pipeline.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pipe(start, end)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{
    Character vector of length 2 containing (1) module name which
    produces the appropriate output (2) name of output. 
%%     ~~Describe \code{start} here~~
}
  \item{end}{
    Character vector of length 2 containing (1) module name which
    contains the appropriate output (2) name of input.
%%     ~~Describe \code{end} here~~
}
}
\details{
  A pipe connects one module's output (start) to another module's input
  (end). This function requires a \sQuote{start} character vector of the form
  \code{c("ORIGIN_MODULE_NAME", "OUTPUT_NAME")} and an \sQuote{end} character
  vector of the form \code{c("TARGET_MODULE_NAME", "INPUT_NAME")}.
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