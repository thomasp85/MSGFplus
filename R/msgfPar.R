#' @include aaa.R
#' @include generics.R
#' @include msgfParChargeRange.R
#' @include msgfParEnzyme.R
#' @include msgfParFragmentation.R
#' @include msgfParInstrument.R
#' @include msgfParIsotopeError.R
#' @include msgfParLengthRange.R
#' @include msgfParMatches.R
#' @include msgfParModificationList.R
#' @include msgfParNtt.R
#' @include msgfParProtocol.R
#' @include msgfParTda.R
#' @include msgfParTolerance.R
NULL

#' A class to contain parameters used in an MSGF+ analysis
#' 
#' This class collects and stores parameters for an MSGF+ analysis and is the starting point for peptide identification
#' 
#' This class contains a range of other classes, each handling a different set of parameters. Often these classes are simple containers that
#' only takes care of errorchecking and generating command line arguments, but in some cases, as with msgfParModificationList, the class is
#' a bit more complex.
#' 
#' @name msgfPar-class
#' 
#' @section Objects from the class:
#' Objects can be created using the \code{\link{msgfPar}} constructor, or with \code{\link{msgfParGUI}} for a simple graphical user interface
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{database}:}{The location of the database fasta file used for the analysis.}
#'  \item{\code{tolerance}:}{An \code{msgfParTolerance} object holding the m/z tolerance used in the search.}
#'  \item{\code{isotopeError}:}{An \code{msgfParIsotopeError} object holding the isotope errors permitted in the search.}
#'  \item{\code{tda}:}{An \code{msgfParTda} object saying whether FDR should be estimated using the target-decoy approach.}
#'  \item{\code{fragmentation}:}{An \code{msgfParFragmentation} object holding the type of fragmentation expected from the experiment.}
#'  \item{\code{instrument}:}{An \code{msgfParInstrument} object holding which type of instrument was used for collecting the data.}
#'  \item{\code{enzyme}:}{An \code{msgfParEnzyme} object holding which enzyme was used for digestion}
#'  \item{\code{protocol}:}{An \code{msgfParProtocol} object defining whether a specific protocol should be used in the search.}
#'  \item{\code{ntt}:}{An \code{msgfParNtt} object defining the number of tolerable termini allowed in the peptides.}
#'  \item{\code{modification}:}{An \code{msgfParModificationList} object holding the modifications accepted in the search.}
#'  \item{\code{lengthRange}:}{An \code{msgfParLengthRange} object setting the limits on the peptide length in residues that the search allows.}
#'  \item{\code{chargeRange}:}{An \code{msgfParChargeRange} object defining which charges should be included in the search.}
#'  \item{\code{matches}:}{An \code{msgfParMatches} object defining the number of matches per PSM that gets reported in the output.}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{\link{getMSGFpar}}:}{Get a string representation of the object that can be directly used in a system call.}
#'  \item{\code{\link{runMSGF}}:}{Start an MSGF+ search based on the msgfPar object.}
#'  \item{\code{\link{database}}:}{Retrieves the database location from the msgfPar object.}
#'  \item{\code{\link{database<-}}:}{Sets the database location for the msgfPar object.}
#' }
#' 
#' @seealso \code{\link{msgfPar}} \code{\link{msgfParGUI}}
#' 
#' @references \url{http://proteomics.ucsd.edu/Software/MSGFPlus.html}
#' 
#' @exportClass msgfPar
#' @rdname msgfPar-class
#' 
setClass(
	Class='msgfPar',
	representation=representation(
		database = 'character',
		tolerance = 'msgfParTolerance',
		isotopeError = 'msgfParIsotopeError',
		tda = 'msgfParTda',
		fragmentation = 'msgfParFragmentation',
		instrument = 'msgfParInstrument',
		enzyme = 'msgfParEnzyme',
		protocol = 'msgfParProtocol',
		ntt = 'msgfParNtt',
		modification = 'msgfParModificationList',
		lengthRange = 'msgfParLengthRange',
		chargeRange = 'msgfParChargeRange',
		matches = 'msgfParMatches'
	),
	validity=function(object){
		if(object@database != ''){
			if(!file.exists(object@database)){
				stop('No file at specified location')
			} else {}
			splitName <- strsplit(object@database, '\\.')[[1]]
			if(!splitName[length(splitName)] %in% c('fasta', 'fa')){
				stop('Database must be a fasta file')
			}	
		}
		return(TRUE)
	},
	prototype=prototype(
		database='',
		tolerance=msgfParTolerance(),
		isotopeError=msgfParIsotopeError(),
		tda=msgfParTda(),
		fragmentation=msgfParFragmentation(),
		instrument=msgfParInstrument(),
		enzyme=msgfParEnzyme(),
		protocol=msgfParProtocol(),
		ntt=msgfParNtt(),
		modification=msgfParModificationList(),
		lengthRange=msgfParLengthRange(),
		chargeRange=msgfParChargeRange(),
		matches=msgfParMatches()
	)
)

#' Show method for msgfPar objects
#' 
#' This method report a general summary of the parameters specified in the msgfPar object
#' 
#' @param object An msgfPar object
#' 
#' @return A description of the content of the msgfPar object
#' 
#' @seealso \code{\link{msgfPar-class}}
#' 
setMethod(
	'show', 'msgfPar',
	function(object){
		if(length(object) == 0){
			cat('An empty msgfPar object\n')
		} else {
			cat('An msgfPar object\n\n')
			cat('Database:                   ', object@database, '\n', sep='')
			if(length(object@tolerance) != 0) {cat('Tolerance:                  '); show(object@tolerance)}
			if(length(object@isotopeError) != 0) {cat('Isotope error range:        '); show(object@isotopeError)}
			if(length(object@tda) != 0) {cat('TDA:                        '); show(object@tda)}
			if(length(object@fragmentation) != 0) {cat('Fragmentation:              '); show(object@fragmentation)}
			if(length(object@instrument) != 0) {cat('Instrument:                 '); show(object@instrument)}
			if(length(object@enzyme) != 0) {cat('Enzyme:                     '); show(object@enzyme)}
			if(length(object@protocol) != 0) {cat('Protocol:                   '); show(object@protocol)}
			if(length(object@ntt) != 0) {cat('No. tolerable termini:      '); show(object@ntt)}
			if(length(object@lengthRange) != 0) {cat('Peptide length:             '); show(object@lengthRange)}
			if(length(object@chargeRange) != 0) {cat('Charges:                    '); show(object@chargeRange)}
			if(length(object@matches) != 0) {cat('Number of reported matches: '); show(object@matches)}
			if(length(object@modification) != 0) {cat('\nModifications:\n\n'); show(object@modification)}
		}
	}
)

#' Report the length of an msgfPar object
#' 
#' The length of an msgfPar object is defined as 1 if a database is present and 0 otherwise
#' 
#' @param x An msgfPar object
#' 
#' @return A \code{numeric} giving the lengthof the object
#' 
#' @seealso \code{\link{msgfPar-class}}
#' 
#' @aliases length,msgfPar-method
#' 
setMethod(
	'length', 'msgfPar',
	function(x){
		if(x@database==''){
			0
		} else {
			1
		}
	}
)

#' @rdname getMSGFpar-methods
#' @aliases getMSGFpar,msgfPar-method
#' 
setMethod(
	'getMSGFpar', 'msgfPar',
	function(object){
		par <- list(object@tolerance, object@isotopeError, object@tda, object@fragmentation, object@instrument, object@enzyme, object@protocol, object@ntt, object@modification, object@lengthRange, object@chargeRange, object@matches)
		par <- sapply(par, getMSGFpar)
    par <- par[par!='']
		if(Sys.info()["sysname"] == 'Windows'){
			db <- paste('\"', object@database, '\"', sep='')
		} else {
			db <- gsub(' ', '\\ ', object@database, fixed=T)
		}
		ans <- paste('-d', db, paste(par, collapse=' '))
		ans
	}
)

#' @rdname database-methods
#' @aliases database,msgfPar-method
#' 
setMethod(
	'database', 'msgfPar',
	function(object){
		object@database
	}
)

#' @rdname database-methods
#' @aliases database<-,msgfPar-method
#' 
setReplaceMethod(
	'database', 'msgfPar',
	function(object, value){
		object@database <- value
		validObject(object)
		object
	}
)

#' @param rawfiles A character vector holding the filepath to the spectrum files to be analysed (currently supported formats: *.mzML, *.mzXML, *.mgf, *.ms2, *.pkl or *_dta.txt)
#' 
#' @param savenames An optinal vector of same length as rawfiles. Specifies the name used to save the results. If omitted the results will be saved with the same name as the rawfile, but with an .mzid extension.
#' 
#' @param import Logical (default=TRUE). Should the results be imported in to R after the analysis is finished.
#' 
#' @param memory An integer (default=10000). How much memory should be allocated to the java virtual machine during execution (in mb)
#' 
#' @rdname runMSGF-methods
#' @aliases runMSGF,msgfPar-method
setMethod(
  'runMSGF', 'msgfPar',
  function(object, rawfiles, savenames, import=TRUE, memory=10000){
    if(length(rawfiles) != length(savenames) & !missing(savenames)){
      stop('Number of raw files must correspond to number of savenames')
    } else {}
    systemCall <- getMSGFpar(object)
    if(missing(savenames)){
      savenames <- paste(sapply(strsplit(rawfiles,"\\."), function(x) paste(x[1:(length(x)-1)], collapse=".")), '.mzid', sep='')
    } else{}
    for(i in 1:length(rawfiles)){
      if(basename(savenames[i]) == savenames[i]){
        savenames[i] <- file.path(getwd(), savenames[i])
      }
      fileCall <- createFileCall(rawfiles[i], savenames[i], database[i])
      systemCall <- paste0('java -Xmx', memory, 'M -jar ', R.home(component='library/MSGFplus/java/MSGFplus.jar'), ' ', fileCall, ' ', systemCall)
      system(systemCall)
    }
    if(import){
      ans <- list()
      for(i in 1:length(savenames)){
        ans[[i]] <- mzID(savenames[i])
      }
      ans
    }
  }
)

#' Constructor for the msgfPar class
#' 
#' 
msgfPar <- function(database, tolerance, isotopeError, tda, fragmentation, instrument, enzyme, protocol, ntt, modification, lengthRange, chargeRange, matches){
	if(missing(database)) database <- ''
	call <- list()
	call$Class <- 'msgfPar'
	call$database <- database
	if(!missing(tolerance)){
		if(class(tolerance) == 'msgfParTolerance'){
			call$tolerance <- tolerance
		} else {
			call$tolerance <- do.call('msgfParTolerance', as.list(tolerance))
		}
	} else {}
	if(!missing(isotopeError)){
		if(class(isotopeError) == 'msgfParIsotopeError'){
			call$isotopeError <- isotopeError
		} else {
			call$isotopeError <- msgfParIsotopeError(unlist(isotopeError))
		}
	} else {}
	if(!missing(tda)){
		if(class(tda) == 'msgfParTda'){
			call$tda <- tda
		} else {
			call$tda <- msgfParTda(unlist(tda))
		}
	} else {}
	if(!missing(fragmentation)){
		if(class(fragmentation) == 'msgfParFragmentation'){
			call$fragmentation <- fragmentation
		} else {
			call$fragmentation <- msgfParFragmentation(unlist(fragmentation))
		}
	} else {}
	if(!missing(instrument)){
		if(class(instrument) == 'msgfParInstrument'){
			call$instrument <- instrument
		} else {
			call$instrument <- msgfParInstrument(unlist(instrument))
		}
	} else {}
	if(!missing(enzyme)){
		if(class(enzyme) == 'msgfParEnzyme'){
			call$enzyme <- enzyme
		} else {
			call$enzyme <- msgfParEnzyme(unlist(enzyme))
		}
	} else {}
	if(!missing(protocol)){
		if(class(protocol) == 'msgfParProtocol'){
			call$protocol <- protocol
		} else {
			call$protocol <- msgfParProtocol(unlist(protocol))
		}
	} else {}
	if(!missing(ntt)){
		if(class(ntt) == 'msgfParNtt'){
			call$ntt <- ntt
		} else {
			call$ntt <- msgfParNtt(unlist(ntt))
		}
	} else {}
	if(!missing(modification)){
		if(class(modification) == 'msgfParModificationList'){
			call$modification <- modification
		} else {
			call$modification <- do.call('msgfParModificationList', modification)
		}
	} else {}
	if(!missing(lengthRange)){
		if(class(lengthRange) == 'msgfParLengthRange'){
			call$lengthRange <- lengthRange
		} else {
			call$lengthRange <- msgfParLengthRange(unlist(lengthRange))
		}
	} else {}
	if(!missing(chargeRange)){
		if(class(chargeRange) == 'msgfParChargeRange'){
			call$chargeRange <- chargeRange
		} else {
			call$chargeRange <- msgfParChargeRange(unlist(chargeRange))
		}
	} else {}
	if(!missing(matches)){
		if(class(matches) == 'msgfParMatches'){
			call$matches <- matches
		} else {
			call$matches <- msgfParMatches(unlist(matches))
		}
	} else {}
	do.call('new', call)
}
msgfParFromID <- function(file){
	parsedMZID <- xmlTreeParse(file, useInternalNodes=TRUE)
	rootMZID <- xmlRoot(parsedMZID)
	ans <- list()
	ans$database <- xmlAttrs(rootMZID[['DataCollection']][['Inputs']][['SearchDatabase']])['location']
	if(!file.exists(ans$database)){
		ans$database <- file.path(dirname(file), basename(ans$database))
		if(!file.exists(ans$database)){
			ans$database <- ''
		} else {}
	} else {}
	for(i in 1:length(xmlSApply(rootMZID[['AnalysisProtocolCollection']][['SpectrumIdentificationProtocol']][['AdditionalSearchParams']], xmlName))){
		if(xmlName(rootMZID[['AnalysisProtocolCollection']][['SpectrumIdentificationProtocol']][['AdditionalSearchParams']][[i]]) == 'userParam'){
			param <- xmlAttrs(rootMZID[['AnalysisProtocolCollection']][['SpectrumIdentificationProtocol']][['AdditionalSearchParams']][[i]])
			if(param['name'] == 'TargetDecoyApproach'){
				ans$tda <- as.logical(param['value'])
			} else if(param['name'] == 'MinIsotopeError'){
				ans$isotopeError[1] <- as.numeric(param['value'])
			} else if(param['name'] == 'MaxIsotopeError'){
				ans$isotopeError[2] <- as.numeric(param['value'])
			} else if(param['name'] == 'FragmentMethod'){
				ans$fragmentation <- param['value']
			} else if(param['name'] == 'Instrument'){
				ans$instrument <- param['value']
			} else if(param['name'] == 'NumTolerableTermini'){
				ans$ntt <- as.numeric(param['value'])
			} else if(param['name'] == 'NumMatchesPerSpec'){
				ans$matches <- as.numeric(param['value'])
			} else if(param['name'] == 'MinPepLength'){
				ans$lengthRange[1] <- as.numeric(param['value'])
			} else if(param['name'] == 'MaxPepLength'){
				ans$lengthRange[2] <- as.numeric(param['value'])
			} else if(param['name'] == 'MinCharge'){
				ans$chargeRange[1] <- as.numeric(param['value'])
			} else if(param['name'] == 'MaxCharge'){
				ans$chargeRange[2] <- as.numeric(param['value'])
			} else {}
		} else {}
	}
	ans$isotopeError <- seq(ans$isotopeError[1], ans$isotopeError[2])
	ans$enzyme <- xmlAttrs(rootMZID[['AnalysisProtocolCollection']][['SpectrumIdentificationProtocol']][['Enzymes']][['Enzyme']][['EnzymeName']][['cvParam']])['name']
	tolerance <- xmlApply(rootMZID[['AnalysisProtocolCollection']][['SpectrumIdentificationProtocol']][['ParentTolerance']], function(x){
				ans <- as.numeric(xmlAttrs(x)['value'])
				if(xmlAttrs(x)['name'] == 'search tolerance plus value'){
					names(ans) <- 'high'
				} else if(xmlAttrs(x)['name'] == "search tolerance minus value"){
					names(ans) <- 'low'
				} else {}
				ans
			})
	names(tolerance) <- NULL
	tolerance <- as.list(unlist(tolerance))
	tolUnit <- xmlAttrs(rootMZID[['AnalysisProtocolCollection']][['SpectrumIdentificationProtocol']][['ParentTolerance']][['cvParam']])['unitName']
	if(tolower(tolUnit) == 'parts per million'){
		tolerance$unit <- 'ppm'
	} else if(tolower(tolUnit) == 'dalton'){
		tolerance$unit <- 'Da'
	} else {}
	ans$tolerance <- tolerance
	modifications <- xmlApply(rootMZID[['AnalysisProtocolCollection']][['SpectrumIdentificationProtocol']][['ModificationParams']], function(x){
		ans <- list()
		ans$residues <- sub('\\.', '*', xmlAttrs(x)['residues'])
		ans$mass <- as.numeric(xmlAttrs(x)['massDelta'])
		ans$type <- if(as.logical(xmlAttrs(x)['fixedMod'])) 'fix' else 'opt'
		ans$name <- xmlAttrs(x[['cvParam']])['name']
		if(is.null(x[['SpecificityRules']])){
			ans$position <- 'any'
		} else {
			if(xmlAttrs(x[['SpecificityRules']][['cvParam']])['name'] == 'modification specificity protein N-term'){
				ans$position <- 'prot-N-term'
			} else if(xmlAttrs(x[['SpecificityRules']][['cvParam']])['name'] == 'modification specificity protein C-term'){
				ans$position <- 'prot-C-term'
			} else if(xmlAttrs(x[['SpecificityRules']][['cvParam']])['name'] == 'modification specificity N-term'){
				ans$position <- 'N-term'
			} else if(xmlAttrs(x[['SpecificityRules']][['cvParam']])['name'] == 'modification specificity C-term'){
				ans$position <- 'C-term'
			} else {}
		}
		do.call('msgfParModification', ans)
	})
	modifications <- list(nMod=2, modifications=modifications)
	ans$modification <- modifications
	free(parsedMZID)
	do.call('msgfPar', ans)
}