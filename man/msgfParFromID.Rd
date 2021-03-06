% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msgfPar.R
\name{msgfParFromID}
\alias{msgfParFromID}
\title{Extract parameters from mzIdentML result file}
\usage{
msgfParFromID(file)
}
\arguments{
\item{file}{The mzIdentML file to extract the parameters from}
}
\value{
An msgfPar object with parameters matching the input file
}
\description{
This function analyses an mzIdentML file generated using MS-GF+ and returns 
an msgfPar object with parameters matching the ones used to generate the 
mzIdentML file. If the mzIdentML file does not origin from an MS-GF+ analysis
it throws an error.
}
\details{
NOTE: At the moment the number of allowed modifications per peptide is not
written to the result file and can thus not be extracted. It defaults to 2
}
\examples{
\dontrun{
parameters <- msgfParFromID('result1.mzid')
}

}
\seealso{
\code{\link{msgfPar-class}} \code{\link{msgfPar}}
}
