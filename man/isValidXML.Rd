% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{isValidXML}
\alias{isValidXML}
\title{Check module and pipeline XML for validity}
\usage{
isValidXML(file, type)
}
\arguments{
\item{file}{XML file to be validated}

\item{type}{\dQuote{module} of \dQuote{pipeline}}
}
\value{
TRUE if valid
}
\description{
Check validity of an XML document against an XML Schema.
}
\details{
Default schema is pipeline.xsd, which includes module.xsd.
}

