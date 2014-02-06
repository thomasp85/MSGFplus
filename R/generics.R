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

#' Runs MS-GF+ based on the given msgfPar object
#' 
#' This function assembles a system call based on the parameters specified in
#' the object and the arguments given in the function call. By default the
#' function uses the MS-GF+ jar file bundled with this package, but it is
#' possible to specify an alternative location using the msgfPar argument.
#' Version compatibility can not be assured in this case though.
#' 
#' @param rawfiles A character vector holding the filepath to the spectrum files to be analysed (currently supported formats: *.mzML, *.mzXML, *.mgf, *.ms2, *.pkl or *_dta.txt)
#' 
#' @param savenames An optinal vector of same length as rawfiles. Specifies the name used to save the results. If omitted the results will be saved with the same name as the rawfile, but with an .mzid extension.
#' 
#' @param import Logical (default=TRUE). Should the results be imported in to R after the analysis is finished.
#' 
#' @param memory An integer (default=10000). How much memory should be allocated to the java virtual machine during execution (in mb)
#' 
#' @param msgfPath The path to an alternative MSGFPlus.jar file if the bundled one is not desired
#' 
#' @return If import=TRUE a list of mzID object otherwise NULL
#'  
#' @export
#' 
#' @docType methods
#' @rdname runMSGF-methods
#' @seealso \code{\link[mzID]{mzID}}
#' 
setGeneric(
  'runMSGF',
  def=function(object, rawfiles, savenames, import, memory, msgfPath){standardGeneric('runMSGF')}
)