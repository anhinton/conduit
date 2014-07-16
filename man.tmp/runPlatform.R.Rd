\name{runPlatform.R}
\alias{runPlatform.R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runPlatform.R(module, inputs, modulePath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{module}{
%%     ~~Describe \code{module} here~~
}
  \item{inputs}{
%%     ~~Describe \code{inputs} here~~
}
  \item{modulePath}{
%%     ~~Describe \code{modulePath} here~~
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
function (module, inputs, modulePath) 
{
    sourceScript <- lapply(module$sources, function(x) {
        x["value"]
    })
    sourceScript <- unlist(sourceScript)
    directoryScript <- paste0("setwd(\"", modulePath, "\")")
    inputScript <- outputScript <- character(1)
    inputScript <- if (length(module$inputs)) {
        sapply(module$inputs, function(x) {
            inputName <- x["name"]
            type <- x["type"]
            fromFile <- inputs[[inputName]]
            input <- if (type == "internal") {
                paste0(inputName, " <- readRDS(\"", fromFile, 
                  "\")")
            }
            else if (type == "external") {
                paste0(inputName, " <- \"", fromFile, "\"")
            }
        })
    }
    outputScript <- if (length(module$outputs)) {
        sapply(module$outputs, function(x) {
            type <- x["type"]
            if (type == "internal") {
                name <- x["name"]
                paste0("saveRDS(", name, ", file=\"", name, ".rds\")")
            }
            else {
                character(1)
            }
        })
    }
    moduleScript <- c(directoryScript, inputScript, sourceScript, 
        outputScript)
    scriptPath <- file.path(modulePath, "script.R")
    scriptFile <- file(scriptPath)
    writeLines(moduleScript, scriptFile)
    close(scriptFile)
    systemCall <- paste0("Rscript ", scriptPath)
    try(system(systemCall))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
