\docType{methods}
\name{runMSGF}
\alias{runMSGF}
\alias{runMSGF,msgfPar-method}
\title{Runs MS-GF+ based on the given msgfPar object}
\usage{
runMSGF(object, rawfiles, savenames, import, memory, msgfPath)

\S4method{runMSGF}{msgfPar}(object, rawfiles, savenames, import = TRUE,
  memory = 10000, msgfPath)
}
\arguments{
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

  \item{msgfPath}{The path to an alternative MSGFPlus.jar
  file if the bundled one is not desired}
}
\value{
If import=TRUE a list of mzID object otherwise NULL
}
\description{
This function assembles a system call based on the
parameters specified in the object and the arguments given
in the function call. By default the function uses the
MS-GF+ jar file bundled with this package, but it is
possible to specify an alternative location using the
msgfPar argument. Version compatibility can not be assured
in this case though.
}
\seealso{
\code{\link[mzID]{mzID}}
}

