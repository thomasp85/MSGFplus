\docType{methods}
\name{getMSGFpar}
\alias{getMSGFpar}
\alias{getMSGFpar,msgfPar-method}
\alias{getMSGFpar,msgfParChargeRange-method}
\title{Get a string representation of an msgfPar-related object}
\usage{
getMSGFpar(object)

\S4method{getMSGFpar}{msgfPar}(object)

\S4method{getMSGFpar}{msgfParChargeRange}(object)
}
\arguments{
  \item{object}{An msgfPar object or a related object}
}
\value{
A string that can be appended to a \code{system()} call to
specify the parameters for the MSGF+ analysis
}
\description{
The string representation is defined as the arguments that
should get appended to the call when running MSGF+ in the
terminal/command prompt
}
\seealso{
\code{\link{msgfPar-class}}
}

