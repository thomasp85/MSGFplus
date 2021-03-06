% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msgfParFragmentation.R
\docType{class}
\name{msgfParFragmentation-class}
\alias{msgfParFragmentation-class}
\alias{show,msgfParFragmentation-method}
\alias{length,msgfParFragmentation-method}
\alias{getMSGFpar,msgfParFragmentation-method}
\alias{msgfParFragmentation}
\title{A class handling Fragmentation types}
\usage{
\S4method{show}{msgfParFragmentation}(object)

\S4method{length}{msgfParFragmentation}(x)

\S4method{getMSGFpar}{msgfParFragmentation}(object)

msgfParFragmentation(method)
}
\arguments{
\item{object}{An msgfParFragmentation object}

\item{x}{An msgfParFragmentation object}

\item{method}{An integer specifying the method}
}
\value{
For length() An integer.

For getMSGFpar() A string.

For msgfParFragmentation() An msgfParFragmentation object.
}
\description{
This class defines a fragmentation type and provides methods to get correct 
system call parameters.
}
\section{Methods (by generic)}{
\itemize{
\item \code{show}: Short summary of msgfParFragmentation object

\item \code{length}: Report the length of an msgfParFragmentation 
object

\item \code{getMSGFpar}: Get \code{\link[base]{system}} compliant 
function call
}}

\section{Slots}{

\describe{
\item{\code{method}}{An integer between 0 and 4 giving the selected method}
}}

\examples{
fragmentation <- msgfParFragmentation(1)
fragmentation <- msgfParFragmentation('CID')

}
\seealso{
Other msgfParClasses: \code{\link{msgfPar-class}},
  \code{\link{msgfParChargeRange-class}},
  \code{\link{msgfParEnzyme-class}},
  \code{\link{msgfParInstrument-class}},
  \code{\link{msgfParIsotopeError-class}},
  \code{\link{msgfParLengthRange-class}},
  \code{\link{msgfParMatches-class}},
  \code{\link{msgfParModification-class}},
  \code{\link{msgfParModificationList-class}},
  \code{\link{msgfParNtt-class}},
  \code{\link{msgfParProtocol-class}},
  \code{\link{msgfParTda-class}},
  \code{\link{msgfParTolerance-class}},
  \code{\link{msgfPar}}
}
