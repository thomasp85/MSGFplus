\docType{methods}
\name{database}
\alias{database}
\alias{database,msgfPar-method}
\alias{database<-}
\title{Get and set the location of the database file as specified in the msgfPar object}
\usage{
database(object)

database(object) <- value

\S4method{database}{msgfPar}(object)
}
\arguments{
  \item{object}{An msgfPar object}

  \item{value}{A string with the filepath to the database}
}
\value{
A string giving the file path to the database
}
\description{
This function extracts or replaces the string holding the
database location in the msgfPar object
}
\seealso{
\code{\link{msgfPar-class}}
}

