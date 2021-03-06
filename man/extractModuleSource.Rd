% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/script.R
\name{extractModuleSource.scriptVessel}
\alias{extractModuleSource}
\alias{extractModuleSource.fileVessel}
\alias{extractModuleSource.scriptVessel}
\alias{extractModuleSource.urlVessel}
\title{Extract a module's source script}
\usage{
\method{extractModuleSource}{scriptVessel}(moduleSource)

\method{extractModuleSource}{fileVessel}(moduleSource)

\method{extractModuleSource}{urlVessel}(moduleSource)

extractModuleSource(moduleSource)
}
\arguments{
\item{moduleSource}{source slot of \code{module} object}
}
\value{
character vector of source script
}
\description{
Extract a module's source script
}
\section{Methods (by class)}{
\itemize{
\item \code{scriptVessel}: Extract a module's source script
from a scriptVessel

\item \code{fileVessel}: Extract a module's source script
from a fileVessel

\item \code{urlVessel}: Extract a module's source script
from a urlVessel
}}
\seealso{
\code{executeScript}
}

