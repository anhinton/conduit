% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/result.R
\name{resultSource}
\alias{resultSource}
\title{Return a \code{moduleSource} object for use in a
\code{moduleResult} object.}
\usage{
resultSource(output, modulePath)
}
\arguments{
\item{output}{\code{output} object}

\item{modulePath}{file path to module output}
}
\value{
\code{moduleSouce} object or NULL
}
\description{
This function constructs an appropriate \code{moduleSource} object
from the \code{output} object created when executing a module
source.
}

