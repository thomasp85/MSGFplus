#' Get a string representation of an msgfPar-related object
#' 
#' The string representation is defined as the arguments that should get
#' appended to the call when running MSGF+ in the terminal/command prompt
#' 
#' @param object An msgfPar object or a related object
#' 
#' @return A string that can be appended to a \code{system()} call to specify 
#' the parameters for the MSGF+ analysis
#' 
#' @seealso \code{\link{msgfPar-class}}
#' 
#' @examples
#' parameters <- msgfPar(
#'                       database=system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'),
#'                       tolerance='20 ppm',
#'                       instrument='TOF',
#'                       enzyme='Lys-C'
#'                      )
#' getMSGFpar(parameters)
#' 
#' @export
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
#' @param rawfiles A character vector holding the filepath to the spectrum files 
#' to be analysed (currently supported formats: *.mzML, *.mzXML, *.mgf, *.ms2, 
#' *.pkl or *_dta.txt)
#' 
#' @param savenames An optinal vector of same length as rawfiles. Specifies the 
#' name used to save the results. If omitted the results will be saved with the 
#' same name as the rawfile, but with an .mzid extension.
#' 
#' @param import Logical (default=TRUE). Should the results be imported in to R 
#' after the analysis is finished.
#' 
#' @param memory An integer (default=10000). How much memory should be allocated 
#' to the java virtual machine during execution (in mb)
#' 
#' @param async An Logical (default=FALSE). Should MS-GF+ be run asynchronously?
#' 
#' @param msgfPath The path to an alternative MSGFPlus.jar file if the bundled 
#' one is not desired
#' 
#' @return If import=TRUE a list of mzID object otherwise NULL
#' 
#' @examples
#' \dontrun{
#' parameters <- msgfPar(
#'                       database=system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'),
#'                       tolerance='20 ppm',
#'                       instrument='TOF',
#'                       enzyme='Lys-C'
#'                      )
#' runMSGF(parameters, c('file1.mzML', 'file2.mzML'))
#' }
#' 
#' @export
#' 
#' @seealso \code{\link[mzID]{mzID}}
#' 
setGeneric(
  'runMSGF',
  def=function(object, rawfiles, savenames, import, memory, async, msgfPath){standardGeneric('runMSGF')}
)

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



#' Get and set database in msgfPar objects
#' 
#' These functions allow you to retrieve and set the location of the database 
#' fasta file in the msgfPar object of interest
#' 
#' @param object An msgfPar object
#' 
#' @return In case of the getter a character vector with the location of the 
#' database file
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar()
#' db(parameters) <- system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta')
#' db(parameters)
#' 
#' @export
#' 
setGeneric(
    'db',
    def=function(object){standardGeneric('db')}
)

#' @rdname db
#' 
#' @param value A string matching the location of a fasta file
#' 
#' @export
#' 
setGeneric(
    'db<-',
    def=function(object, value){standardGeneric('db<-')}
)

#' Get and set the charge range in msgfPar objects
#' 
#' These functions allow you to retrieve and set the charge range in the msgfPar 
#' object of interest
#' 
#' @param object An msgfPar object
#' 
#' @return In case of the getter a numeric vector with the named elements 'min'
#' and 'max'
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' chargeRange(parameters) <- c(2, 4)
#' chargeRange(parameters)
#' 
#' @export
#' 
setGeneric(
    'chargeRange',
    def=function(object){standardGeneric('chargeRange')}
)

#' @rdname chargeRange
#' 
#' @param value Either a numeric vector of length 2 or an msgfParChargeRange
#' object
#' 
#' @export
#' 
setGeneric(
    'chargeRange<-',
    def=function(object, value){standardGeneric('chargeRange<-')}
)

#' Get and set enzyme in msgfPar objects
#' 
#' These functions allow you to retrieve and set the enzyme used for digestion
#' during sample treatment.
#' 
#' @param object An msgfPar object
#' 
#' @return In case of the getter a named integer
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' enzyme(parameters) <- 'Trypsin'
#' enzyme(parameters) <- 3
#' enzyme(parameters)
#' 
#' @export
#' 
setGeneric(
    'enzyme',
    def=function(object){standardGeneric('enzyme')}
)
#' @rdname enzyme
#' 
#' @param value Either an integer, string or msgfParEnzyme object
#' 
#' @export
#' 
setGeneric(
    'enzyme<-',
    def=function(object, value){standardGeneric('enzyme<-')}
)

#' Get and set fragmentation in msgfPar objects
#' 
#' These functions allow you to retrieve and set the fragmentation method used
#' during acquisition
#' 
#' @param object An msgfPar object
#' 
#' @return In case of the getter a named integer
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' fragmentation(parameters) <- 'CID'
#' fragmentation(parameters) <- 3
#' fragmentation(parameters)
#' 
#' @export
#' 
setGeneric(
    'fragmentation',
    def=function(object){standardGeneric('fragmentation')}
)
#' @rdname fragmentation
#' 
#' @param value Either an integer, string or msgfParFragmentation object
#' 
#' @export
#' 
setGeneric(
    'fragmentation<-',
    def=function(object, value){standardGeneric('fragmentation<-')}
)

#' Get and set instrument in msgfPar objects
#' 
#' These functions allow you to retrieve and set the instrument type used for
#' acquisition
#' 
#' @param object An msgfPar object
#' 
#' @return In case of the getter a named integer
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' instrument(parameters) <- 'TOF'
#' instrument(parameters) <- 3
#' instrument(parameters)
#' 
#' @export
#' 
setGeneric(
    'instrument',
    def=function(object){standardGeneric('instrument')}
)
#' @rdname instrument
#' 
#' @param value Either an integer, string or msgfParInstrument object
#' 
#' @export
#' 
setGeneric(
    'instrument<-',
    def=function(object, value){standardGeneric('instrument<-')}
)

#' Get and set isotope error in msgfPar objects
#' 
#' These functions allow you to retrieve and set the isotope error used when 
#' calculating the parent ion error range
#' 
#' @param object An msgfPar object
#' 
#' @return In case of the getter an integer vector
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' isotopeError(parameters) <- c(0, 3)
#' isotopeError(parameters)
#' @export
#' 
setGeneric(
    'isotopeError',
    def=function(object){standardGeneric('isotopeError')}
)
#' @rdname isotopeError
#' 
#' @param value Either an integer vector or an msgfParIsotopeError object
#' 
#' @export
#' 
setGeneric(
    'isotopeError<-',
    def=function(object, value){standardGeneric('isotopeError<-')}
)

#' Get and set peptide length range in msgfPar objects
#' 
#' These functions allow you to retrieve and set the residue length allowed for
#' the peptides search for in MS-GF+
#' 
#' @param object An msgfPar object
#' 
#' @return In case of the getter an integer vector of length 2 giving the lower
#' and upper bounds of the length range
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' lengthRange(parameters) <- c(6, 40)
#' lengthRange(parameters)
#' 
#' @export
#' 
setGeneric(
    'lengthRange',
    def=function(object){standardGeneric('lengthRange')}
)
#' @rdname lengthRange
#' 
#' @param value Either an integer vector or an msgfParLengthRange object
#' 
#' @export
#' 
setGeneric(
    'lengthRange<-',
    def=function(object, value){standardGeneric('lengthRange<-')}
)

#' Get and set the number of matches in msgfPar objects
#' 
#' These functions allow you to retrieve and set the number of matches per 
#' spectrum returned by MS-GF+
#' 
#' @param object An msgfPar object
#' 
#' @return In case of the getter an integer
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' matches(parameters) <- 5
#' matches(parameters)
#' @export
#' 
setGeneric(
    'matches',
    def=function(object){standardGeneric('matches')}
)
#' @rdname matches
#' 
#' @param value Either an integer or an msgfParMatches object
#' 
#' @export
#' 
setGeneric(
    'matches<-',
    def=function(object, value){standardGeneric('matches<-')}
)

#' Get and set cleavage specificity in msgfPar objects
#' 
#' These functions allow you to retrieve and set the quality of cleavage allowed
#' during search in MS-GF+ (number of tolerable termini - ntt)
#' 
#' @param object An msgfPar object
#' 
#' @return In case of the getter an integer between 0 and 2
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' ntt(parameters) <- 2
#' ntt(parameters)
#' 
#' @export
#' 
setGeneric(
    'ntt',
    def=function(object){standardGeneric('ntt')}
)
#' @rdname ntt
#' 
#' @param value An integer or an msgfParNtt object
#' 
#' @export
#' 
setGeneric(
    'ntt<-',
    def=function(object, value){standardGeneric('ntt<-')}
)

#' Get and set protocol in msgfPar objects
#' 
#' These functions allow you to retrieve and set the protocol used during MS-GF+
#' analysis. This allows you to fine tune the analysis in case of labelled or
#' phosphoproteomic analysis
#' 
#' @param object An msgfPar object
#' 
#' @return In case of the getter a named integer
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' protocol(parameters) <- 'Phosphorylation'
#' protocol(parameters) <- 0
#' protocol(parameters)
#' 
#' @export
#' 
setGeneric(
    'protocol',
    def=function(object){standardGeneric('protocol')}
)
#' @rdname protocol
#' 
#' @param value Either an integer, a string or an msgfParProtocol object
#' 
#' @export
#' 
setGeneric(
    'protocol<-',
    def=function(object, value){standardGeneric('protocol<-')}
)

#' Get and set use of target-decoy approach in msgfPar objects
#' 
#' These functions allow you to retrieve and set whether the target-decoy 
#' approach should be used to estimate q-values.
#' 
#' @param object An msgfPar object
#' 
#' @return In case of the getter a boolean indicating whether tda is used or not
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' tda(parameters) <- TRUE
#' tda(parameters)
#' 
#' @export
#' 
setGeneric(
    'tda',
    def=function(object){standardGeneric('tda')}
)
#' @rdname tda
#' 
#' @param value Either a boolean or msgfParTda object
#' 
#' @export
#' 
setGeneric(
    'tda<-',
    def=function(object, value){standardGeneric('tda<-')}
)

#' Get and set the parent tolerance in msgfPar objects
#' 
#' These functions allow you to retrieve and set the tolerance used for matching
#' parent ions to peptides in the database
#' 
#' @param object An msgfPar object
#' 
#' @return For tolerance a character vector with the lower and upper tolerance 
#' limit with unit. For toleranceUnit a string with the unit used. For 
#' toleranceRange a numeric vector with lower and upper tolerance limit.
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' tolerance(parameters) <- c('20 ppm', '20 ppm')
#' toleranceUnit(parameters) <- 'Da'
#' toleranceRange(parameters) <- c(1.5, 1.5)
#' tolerance(parameters)
#' 
#' @export
#' 
setGeneric(
    'tolerance',
    def=function(object){standardGeneric('tolerance')}
)
#' @rdname tolerance
#' 
#' @param value For tolerance a character vector of length 2, each element of 
#' the form '<value> <unit>'. For toleranceUnit a string. For toleranceRange a 
#' numeric vector of length 2.
#' 
#' @export
#' 
setGeneric(
    'tolerance<-',
    def=function(object, value){standardGeneric('tolerance<-')}
)

#' @rdname tolerance
#' 
#' @export
#' 
setGeneric(
    'toleranceRange',
    def=function(object){standardGeneric('toleranceRange')}
)
#' @rdname tolerance
#' 
#' @export
#' 
setGeneric(
    'toleranceRange<-',
    def=function(object, value){standardGeneric('toleranceRange<-')}
)

#' @rdname tolerance
#' 
#' @export
#' 
setGeneric(
    'toleranceUnit',
    def=function(object){standardGeneric('toleranceUnit')}
)
#' @rdname tolerance
#' 
#' @export
#' 
setGeneric(
    'toleranceUnit<-',
    def=function(object, value){standardGeneric('toleranceUnit<-')}
)

#' Get and set the modifications in msgfPar objects
#' 
#' These functions allow you to retrieve and set the specific modifications 
#' allowed on peptides during MS-GF+ search, as well as the number allowed on 
#' each peptide
#' 
#' @param object An msgfPar object
#' 
#' @return For the getter an msgfParModificationList object or an integer (in 
#' the case of nMod)
#' 
#' @family msgfPar-getter_setter
#' 
#' @examples
#' parameters <- msgfPar(system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'))
#' nMod(parameters) <- 2
#' mods(parameters)[[1]] <- msgfParModification(
#'                                                       name='Carbamidomethyl',
#'                                                       composition='C2H3N1O1',
#'                                                       residues='C',
#'                                                       type='fix',
#'                                                       position='any'
#'                                                      )
#' mods(parameters)
#' 
#' @export
#' 
setGeneric(
    'mods',
    def=function(object){standardGeneric('mods')}
)
#' @rdname mods
#' 
#' @param value An msgfParModificationList object or in the case of nMod an 
#' integer
#' 
#' @export
#' 
setGeneric(
    'mods<-',
    def=function(object, value){standardGeneric('mods<-')}
)

#' @rdname mods
#' 
#' @export
#' 
setGeneric(
    'nMod',
    def=function(object){standardGeneric('nMod')}
)
#' @rdname mods
#' 
#' @export
#' 
setGeneric(
    'nMod<-',
    def=function(object, value){standardGeneric('nMod<-')}
)
