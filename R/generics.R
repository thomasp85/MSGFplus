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

#' Runs MS-GF+ based on the given msgfPar object
#' 
#' This function assembles a system call based on the parameters specified in
#' the object and the arguments given in the function call. By default the
#' function uses the MS-GF+ jar file bundled with this package, but it is
#' possible to specify an alternative location using the msgfPar argument.
#' Version compatibility can not be assured in this case though.
#' 
#' @param object An msgfPar object
#' 
#' @param rawfiles A character vector holding the filepath to the spectrum files to be analysed (currently supported formats: *.mzML, *.mzXML, *.mgf, *.ms2, *.pkl or *_dta.txt)
#' 
#' @param savenames An optinal vector of same length as rawfiles. Specifies the name used to save the results. If omitted the results will be saved with the same name as the rawfile, but with an .mzid extension.
#' 
#' @param import Logical (default=TRUE). Should the results be imported in to R after the analysis is finished.
#' 
#' @param memory An integer (default=10000). How much memory should be allocated to the java virtual machine during execution (in mb)
#' 
#' @param async An Logical (default=FALSE). Should MS-GF+ be run asynchronously?
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
  def=function(object, rawfiles, savenames, import, memory, async, msgfPath){standardGeneric('runMSGF')}
)

#' Generic for msgfAsync - Documented with class
#' 
#' @export
#' 
#' @rdname msgfAsync-class
#' 
setGeneric(
    'running',
    def=function(object){standardGeneric('running')}
)
#' @export
#' 
#' @rdname msgfAsync-class
#' 
setGeneric(
    'finished',
    def=function(object){standardGeneric('finished')}
)
#' @export
#' 
#' @rdname msgfAsync-class
#' 
setGeneric(
    'import',
    def=function(object){standardGeneric('import')}
)



#' Get and set the location of the database file as specified in the msgfPar object
#' 
#' This function extracts or replaces the string holding the database location
#' in the msgfPar object
#' 
#' @param object An msgfPar object
#' 
#' @return Depending on the method. See details
#' 
#' @seealso \code{\link{msgfPar-class}}
#' 
#' @export
#' @docType methods
#' @rdname msgfPar_get_set-methods
#' @aliases database
#' @name msgfPar getters amd setters
#' 
setGeneric(
    'database',
    def=function(object){standardGeneric('database')}
)

#' @param value Depending on the method. See details
#' 
#' @export
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'database<-',
    def=function(object, value){standardGeneric('database<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'chargeRange',
    def=function(object){standardGeneric('chargeRange')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'chargeRange<-',
    def=function(object, value){standardGeneric('chargeRange<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'enzyme',
    def=function(object){standardGeneric('enzyme')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'enzyme<-',
    def=function(object, value){standardGeneric('enzyme<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'fragmentation',
    def=function(object){standardGeneric('fragmentation')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'fragmentation<-',
    def=function(object, value){standardGeneric('fragmentation<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'instrument',
    def=function(object){standardGeneric('instrument')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'instrument<-',
    def=function(object, value){standardGeneric('instrument<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'isotopeError',
    def=function(object){standardGeneric('isotopeError')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'isotopeError<-',
    def=function(object, value){standardGeneric('isotopeError<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'lengthRange',
    def=function(object){standardGeneric('lengthRange')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'lengthRange<-',
    def=function(object, value){standardGeneric('lengthRange<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'matches',
    def=function(object){standardGeneric('matches')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'matches<-',
    def=function(object, value){standardGeneric('matches<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'ntt',
    def=function(object){standardGeneric('ntt')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'ntt<-',
    def=function(object, value){standardGeneric('ntt<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'protocol',
    def=function(object){standardGeneric('protocol')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'protocol<-',
    def=function(object, value){standardGeneric('protocol<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'tda',
    def=function(object){standardGeneric('tda')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'tda<-',
    def=function(object, value){standardGeneric('tda<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'tolerance',
    def=function(object){standardGeneric('tolerance')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'tolerance<-',
    def=function(object, value){standardGeneric('tolerance<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'toleranceRange',
    def=function(object){standardGeneric('toleranceRange')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'toleranceRange<-',
    def=function(object, value){standardGeneric('toleranceRange<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'toleranceUnit',
    def=function(object){standardGeneric('toleranceUnit')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'toleranceUnit<-',
    def=function(object, value){standardGeneric('toleranceUnit<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'modifications',
    def=function(object){standardGeneric('modifications')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'modifications<-',
    def=function(object, value){standardGeneric('modifications<-')}
)

#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'nMod',
    def=function(object){standardGeneric('nMod')}
)
#' @export
#' 
#' @rdname msgfPar_get_set-methods
#' 
setGeneric(
    'nMod<-',
    def=function(object, value){standardGeneric('nMod<-')}
)
