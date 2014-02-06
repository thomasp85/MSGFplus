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

#' A class to contain parameters used in an MS-GF+ analysis
#' 
#' This class collects and stores parameters for an MS-GF+ analysis and is the
#' starting point for peptide identification
#' 
#' This class contains a range of other classes, each handling a different set
#' of parameters. Often these classes are simple containers that only takes care
#' of errorchecking and generating command line arguments, but in some cases, as
#' with msgfParModificationList, the class is a bit more complex.
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
#'  \item{\code{\link{runMSGF}}:}{Start an MS-GF+ search based on the msgfPar object.}
#'  \item{\code{\link{database}}:}{Retrieves the database location from the msgfPar object.}
#'  \item{\code{\link{database<-}}:}{Sets the database location for the msgfPar object.}
#' }
#' 
#' @seealso \code{\link{msgfParGUI}}
#' @family msgfParClasses
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
				return('No file at specified location')
			} else {}
			splitName <- strsplit(object@database, '\\.')[[1]]
			if(!splitName[length(splitName)] %in% c('fasta', 'fa')){
				return('Database must be a fasta file')
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
#' This method report a general summary of the parameters specified in the
#' msgfPar object
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
#' The length of an msgfPar object is defined as 1 if a database is present and
#' 0 otherwise
#' 
#' @param x An msgfPar object
#' 
#' @return A \code{numeric} giving the lengthof the object
#' 
#' @seealso \code{\link{msgfPar-class}}
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
#' 
setMethod(
	'database', 'msgfPar',
	function(object){
		object@database
	}
)
setReplaceMethod(
	'database', 'msgfPar',
	function(object, value){
		object@database <- value
		validObject(object)
		object
	}
)


#' @importFrom mzID mzID
#' @rdname runMSGF-methods
#' 
setMethod(
    'runMSGF', 'msgfPar',
    function(object, rawfiles, savenames, import=TRUE, memory=10000, msgfPath){
        if(missing(import)) {
            import <- TRUE
        } else {}
        if(missing(memory)) {
            memory=10000
        } else {}
        if(missing(msgfPath)) {
            msgfPath <- R.home(component='library/MSGFplus/MSGFPlus/MSGFPlus.jar')
        } else {}
        if(!missing(savenames) && length(rawfiles) != length(savenames)){
            stop('Number of raw files must correspond to number of savenames')
        } else {}
        if(missing(savenames)){
            savenames <- paste(sapply(strsplit(rawfiles,"\\."), function(x) paste(x[1:(length(x)-1)], collapse=".")), '.mzid', sep='')
        } else{}
        
        parameterCall <- getMSGFpar(object)
        
        for(i in 1:length(rawfiles)){
            if(basename(savenames[i]) == savenames[i]){
                savenames[i] <- file.path(getwd(), savenames[i])
            }
            fileCall <- createFileCall(rawfiles[i], savenames[i])
            systemCall <- paste0('java -Xmx', memory, 'M -jar ', msgfPath, ' ', fileCall, ' ', parameterCall)
            cat(systemCall)
            system(systemCall)
        }
        if(import){
            ans <- list()
            for(i in 1:length(savenames)){
                ans[[i]] <- mzID(savenames[i])
            }
            ans
        } else {
            NULL
        }
    }
)

#' Constructor for the msgfPar class
#' 
#' This function creates an msgfPar object with the specified parameters. If 
#' some parameters have not been specified they will not be part of the MS-GF+ 
#' call and MS-GF+'s own defaults kicks in; Consult the MS-GF+ documentation for
#' these. Note however that at least a database file is required to run an 
#' analysis.
#' 
#' Please consult the MS-GF+ documentation for full description of the parameters
#' 
#' @param database The location of the fasta file to use as search database
#' 
#' @param tolerance The parent ion tolerance to use. In simple cases a string in the form '20ppm' or '1Da' or an msgfParTolerance object if asymmetric tolerance is desired
#' 
#' @param isotopeError The range of isotope errors used to correct for non-monoisotopic peaks. Either a numeric vector of length 2 specifying the lower and upper bounds of the range, or an msgfParIsotopeError object
#' 
#' @param tda \code{Logical} Should Target-Decoy approach be used to calculate FDR values.
#' 
#' @param fragmentation An integer specifying which fragmentation has been used during data acquisition. See details.
#' 
#' @param instrument An integer specifying the type of instrument used during data acquisition. See details.
#' 
#' @param enzyme An integer or name specifying the enzyme that has been used for protein digestion. See details.
#' 
#' @param protocol An integer or name specifying the type of preparation that has been done for the samples. See details.
#' 
#' @param ntt An integer specifying the cleavage specificity (Number of Tolerable Termini). 2 only allows fully tryptic peptide (if trypsin is used), 1 allows semitryptic peptides and 0 allows unspecific peptides
#' 
#' @param modification An msgfParModificationList object or a list containing the named elements nmod and modification containg respectively an integer with the number of allowed modifications per petide and the modifications to search for as msgfParModification 
#' 
#' @param lengthRange A two element vector containing the lower and upper bounds of the residue length to search for
#' 
#' @param chargeRange A two element vector containing the lower and upper bounds of the charge range to search for
#' 
#' @param matches The number of matches to report per spectrum
#' 
#' 
#' @details
#' Fragmentation is usually specified as an integer according to the following lookup
#' \describe{
#'   \item{0}{As written in the spectrum or CID if no info}
#'   \item{1}{CID}
#'   \item{2}{ETD}
#'   \item{3}{HCD}
#'   \item{4}{Merge spectra from the same precursor}
#' }
#' It is possible to use the full name of the description for a more litteral function call
#' 
#' Instrument can likewise be specified as an integer or as a name according to this list
#' \describe{
#'   \item{0}{LowRes}
#'   \item{1}{HighRes}
#'   \item{2}{TOF}
#'   \item{3}{QExactive}
#' }
#' 
#' Enymes are specified in the same manner using the following list
#' \describe{
#'   \item{0}{Unspecific cleavage}
#'   \item{1}{Trypsin}
#'   \item{2}{Chymotrypsin}
#'   \item{3}{Lys-C}
#'   \item{4}{Lys-N}
#'   \item{5}{glutamyl endopeptidase (Glu-C)}
#'   \item{6}{Arg-C}
#'   \item{7}{Asp-N}
#'   \item{8}{alphaLP}
#'   \item{9}{No cleavage}
#' }
#' 
#' The protocol informs MS-GF+ whether a special sample treatment has been performed as part of the analysis. The protocol is specified according to the following list
#' \describe{
#'   \item{0}{No protocol}
#'   \item{1}{Phosphorylation}
#'   \item{2}{iTRAQ}
#'   \item{3}{iTRAQPhospho}
#' }
#' 
#' @family msgfParClasses
#' 
#' @references \href{http://proteomics.ucsd.edu/Software/MSGFPlus.html}{MS-GF+}
#' 
#' @export
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

#' Extract parameters from mzIdentML result file
#' 
#' This function analyses an mzIdentML file generated using MS-GF+ and returns 
#' an msgfPar object with parameters matching the ones used to generate the 
#' mzIdentML file. If the mzIdentML file does not origin from an MS-GF+ analysis
#' it throws an error.
#' 
#' NOTE: At the moment the number of allowed modifications per peptide is not
#' written to the result file and can thus not be extracted. It defaults to 2
#' 
#' @param file The mzIdentML file to extract the parameters from
#' 
#' @return An msgfPar object with parameters matching the input file
#' 
#' @seealso \code{\link{msgfPar-class}}
#' 
#' @importFrom mzID mzIDparameters
#' 
#' @export
#' 
msgfParFromID <- function(file){
    parameters <- mzIDparameters(path=file)
    if (parameters@software$name[parameters@software$id == 'ID_software'] != 'MS-GF+') {
        stop('Parameters can only be imported if result file has been processed with MS-GF+')
    }
	ans <- list()
	ans$database <- parameters@databaseFile$location
	if(!file.exists(ans$database)){
		ans$database <- file.path(dirname(file), basename(ans$database))
		if(!file.exists(ans$database)){
			ans$database <- ''
		} else {}
	} else {}
    
    ans$tda <- parameters@parameters$TargetDecoyApproach
    ans$isotopeError <- seq(parameters@parameters$MinIsotopeError, parameters@parameters$MaxIsotopeError)
    ans$fragmentation <- as.character(parameters@parameters$FragmentMethod)
    ans$instrument <- as.character(parameters@parameters$Instrument)
    ans$ntt <- parameters@parameters$NumTolerableTermini
    ans$matches <- parameters@parameters$NumMatchesPerSpec
    ans$lengthRange <- c(parameters@parameters$MinPepLength, parameters@parameters$MaxPepLength)
    ans$chargeRange <- c(parameters@parameters$MinCharge, parameters@parameters$MaxCharge)
	ans$enzyme <- parameters@parameters$enzymes$name
    ans$tolerance <- list(
        high=parameters@parameters$ParentTolerance$value[grepl('search tolerance plus value', parameters@parameters$ParentTolerance$name)],
        low=parameters@parameters$ParentTolerance$value[grepl('search tolerance minus value', parameters@parameters$ParentTolerance$name)],
        unit=ifelse(tolower(parameters@parameters$ParentTolerance$unitName[1]) == 'parts per million', 'ppm',
                    ifelse(tolower(parameters@parameters$ParentTolerance$unitName[1]) == 'dalton', 'Da', NULL)
                    )
        )
    modifications <- apply(parameters@parameters$ModificationRules, 1, function(x) {
        x <- lapply(x, type.convert)
        try(msgfParModification(
            name=as.character(x$name), 
            mass=x$massDelta, 
            residues=as.character(x$residues), 
            type=ifelse(x$fixedMod, 'fix', 'opt'), 
            position=ifelse(x$Specificity == 'any', 'any', ifelse(x$Specificity == 'modification specificity N-term', 'N-term')))
        )}
    )
    if (any(sapply(modifications, function(x) {inherits(x, 'try-error')}))) {
        warning('Some modification rules removed due to bad formating')
    }
    modifications <- modifications[sapply(modifications, function(x) {!inherits(x, 'try-error')})]
	ans$modification <- list(nMod=2, modifications=modifications)
	do.call('msgfPar', ans)
}