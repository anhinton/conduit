% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module.R
\name{ioFormat}
\alias{ioFormat}
\title{Create an \code{ioFormat} object.}
\usage{
ioFormat(value, type = "text")
}
\arguments{
\item{value}{Format information}

\item{type}{Method of format specification}
}
\value{
\code{ioFormat} list object
}
\description{
Specify the format of a \code{moduleInput} or \code{moduleOutput} object.
}
\details{
If \code{type} = \dQuote{text}, \code{value} must be a character
vector of length 1.
}
\examples{
i1_format <- ioFormat(value = "CSV file")

}
\seealso{
\code{moduleInput}, \code{moduleOutput}, \code{module}
}

