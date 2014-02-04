\docType{methods}
\name{runMSGF}
\alias{runMSGF}
\alias{runMSGF,msgfPar-method}
\title{Start an MSGF+ analysis based on an msgfPar object}
\usage{
runMSGF(object, ...)

\S4method{runMSGF}{msgfPar}(object, rawfiles, savenames, import = TRUE,
  memory = 10000)
}
\arguments{
  \item{object}{An msgfPar object}

  \item{rawfiles}{A character vector holding the filepath
  to the spectrum files to be analysed (currently supported
  formats: *.mzML, *.mzXML, *.mgf, *.ms2, *.pkl or
  *_dta.txt)}

  \item{savenames}{An optinal vector of same length as
  rawfiles. Specifies the name used to save the results. If
  omitted the results will be saved with the same name as
  the rawfile, but with an .mzid extension.}

  \item{import}{Logical (default=TRUE). Should the results
  be imported in to R after the analysis is finished.}

  \item{memory}{An integer (default=10000). How much memory
  should be allocated to the java virtual machine during
  execution (in mb)}
}
\value{
If the result of the analysis is imported, a list of mzID
objects
}
\description{
This function extracts the parameters defined in an msgfPar
object and starts an MSGF+ analysis through a system() call
}
\seealso{
\code{\link[mzID]{mzID}}
}

