% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module.R
\name{prepareInput}
\alias{prepareInput}
\title{Prepare input object}
\usage{
prepareInput(moduleInput, inputList, outputDirectory, moduleLanguage, location)
}
\arguments{
\item{moduleInput}{\code{moduleInput} object}

\item{inputList}{list of \code{input} objects provided to module}

\item{outputDirectory}{working directory for module execution}

\item{moduleLanguage}{\code{moduleLanguage} object}

\item{location}{location of originating module file}
}
\value{
\code{input} object. Generally a character string
    referencing a file location or URL
}
\description{
This function ensures a \code{module}'s \code{moduleInput}
requirements will be met when executed.
}
\details{
For any \code{moduleInput} wrapping a \code{internalVessel} or
\code{fileVessel} with a relative ref, the relevant \code{input}
object is copied into the module \code{outputDirectory}.

For any \code{moduleInput} that is not found in \code{inputList}
(e.g., when the module is being run directly), the \code{moduleInput}
itself is tried (e.g., a URL vessel should work).
}

