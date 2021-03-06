% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/msgfPar-getters.R
\docType{methods}
\name{db}
\alias{db}
\alias{db<-}
\alias{db,msgfPar-method}
\alias{db<-,msgfPar,character-method}
\title{Get and set database in msgfPar objects}
\usage{
db(object)

db(object) <- value

\S4method{db}{msgfPar}(object)

\S4method{db}{msgfPar,character}(object) <- value
}
\arguments{
\item{object}{An msgfPar object}

\item{value}{A string matching the location of a fasta file}
}
\value{
In case of the getter a character vector with the location of the 
database file
}
\description{
These functions allow you to retrieve and set the location of the database 
fasta file in the msgfPar object of interest
}
\section{Methods (by class)}{
\itemize{
\item \code{msgfPar}: Get the database location

\item \code{object = msgfPar,value = character}: Set the database location
}}

\examples{
parameters <- msgfPar()
db(parameters) <- system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta')
db(parameters)

}
\seealso{
Other msgfPar-getter_setter: \code{\link{chargeRange}},
  \code{\link{enzyme}}, \code{\link{fragmentation}},
  \code{\link{instrument}}, \code{\link{isotopeError}},
  \code{\link{lengthRange}}, \code{\link{matches}},
  \code{\link{mods}}, \code{\link{ntt}},
  \code{\link{protocol}}, \code{\link{tda}},
  \code{\link{tolerance}}
}
