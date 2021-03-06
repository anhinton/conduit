% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module.R
\name{moduleLanguageToXML}
\alias{moduleLanguageToXML}
\title{Create XML corresponding to \code{moduleLanguage} object}
\usage{
moduleLanguageToXML(moduleLanguage, parent = NULL,
  addFinalizer = is.null(parent))
}
\arguments{
\item{moduleLanguage}{\code{moduleLanguage} objects}

\item{parent}{parent XML object}

\item{addFinalizer}{logical add finalizer to free internal xmlDoc}
}
\value{
XML node representing module language
}
\description{
Create XML corresponding to \code{moduleLanguage} object
}
\seealso{
\code{XML::newXMLNode}
}

