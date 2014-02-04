#' Get a string representation of an msgfPar-related object
#' 
#' The string representation is defined as the arguments that should get
#' appended to the call when running MSGF+ in the terminal/command prompt
#' 
#' @param object An msgfPar object or a related object
#' 
#' @return A string that can be appended to a \code{system()} call to specify the parameters for the MSGF+ analysis
#' 
#' @seealso \code{\link{msgfPar-class}}
#' 
#' @export
#' @docType methods
#' @rdname getMSGFpar-methods
#' 
setGeneric(
		'getMSGFpar',
		def=function(object){standardGeneric('getMSGFpar')}
)

#' Get and set the location of the database file as specified in the msgfPar object
#' 
#' This function extracts or replaces the string holding the database location
#' in the msgfPar object
#' 
#' @param object An msgfPar object
#' 
#' @return A string giving the file path to the database
#' 
#' @seealso \code{\link{msgfPar-class}}
#' 
#' @export
#' @docType methods
#' @rdname database-methods
#' 
setGeneric(
		'database',
		def=function(object){standardGeneric('database')}
)

#' @param value A string with the filepath to the database
#' 
#' @export
#' @rdname database-methods
#' 
setGeneric(
		'database<-',
		def=function(object, value){standardGeneric('database<-')}
)

#' Start an MSGF+ analysis based on an msgfPar object
#' 
#' This function extracts the parameters defined in an msgfPar object and starts
#' an MSGF+ analysis through a system() call
#' 
#' @param object An msgfPar object
#' 
#' @return If the result of the analysis is imported, a list of mzID objects
#' 
#' @export
#' 
#' @docType methods
#' @rdname runMSGF-methods
#' @seealso \code{\link[mzID]{mzID}}
#' 
setGeneric(
  'runMSGF',
  def=function(object, ...){standardGeneric('runMSGF')}
)