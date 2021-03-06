% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline.R
\name{calculateInputs}
\alias{calculateInputs}
\title{Returns a named list of input objects}
\usage{
calculateInputs(pipeList, componentList, pipelinePath)
}
\arguments{
\item{pipeList}{List of \code{pipe} objects}

\item{componentList}{List of \code{component} objects}

\item{pipelinePath}{Absolute file path to originating
\code{pipeline} XML file}
}
\value{
named list of lists of \code{input} objects for each
    component
}
\description{
\code{calculateInputs} returns a named list of \code{input} objects
to satisfy component inputs provided by pipes.
}
\details{
List items are named as componentName.inputName
}

