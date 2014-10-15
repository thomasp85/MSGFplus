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
#' @section Objects from the class:
#' Objects can be created using the \code{\link{msgfPar}} constructor, or with 
#' \code{\link{msgfParGUI}} for a simple graphical user interface
#' 
#' @slot database The location of the database fasta file used for the analysis.
#' @slot tolerance An \code{msgfParTolerance} object holding the m/z tolerance 
#' used in the search.
#' @slot isotopeError An \code{msgfParIsotopeError} object holding the isotope 
#' errors permitted in the search.
#' @slot tda An \code{msgfParTda} object saying whether FDR should be estimated 
#' using the target-decoy approach.
#' @slot fragmentation An \code{msgfParFragmentation} object holding the type of
#' fragmentation expected from the experiment.
#' @slot instrument An \code{msgfParInstrument} object holding which type of 
#' instrument was used for collecting the data.
#' @slot enzyme An \code{msgfParEnzyme} object holding which enzyme was used for
#' digestion
#' @slot protocol An \code{msgfParProtocol} object defining whether a specific 
#' protocol should be used in the search.
#' @slot ntt An \code{msgfParNtt} object defining the number of tolerable 
#' termini allowed in the peptides.
#' @slot modification An \code{msgfParModificationList} object holding the 
#' modifications accepted in the search.
#' @slot lengthRange An \code{msgfParLengthRange} object setting the limits on 
#' the peptide length in residues that the search allows.
#' @slot chargeRange An \code{msgfParChargeRange} object defining which charges 
#' should be included in the search.
#' @slot matches An \code{msgfParMatches} object defining the number of matches 
#' per PSM that gets reported in the output.
#' 
#' @seealso \code{\link{msgfParGUI}}
#' @family msgfParClasses
#' 
#' @references \url{http://proteomics.ucsd.edu/Software/MSGFPlus.html}
#' 
#' @export
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

#' @describeIn msgfPar Short summary of msgfPar object
#' 
#' @param object An msgfPar object
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

#' @describeIn msgfPar Report the length of an msgfPar object
#' 
#' @param x An msgfPar object
#' 
#' @return \code{length}: 1 if a database is defined, 0 otherwise.
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

#' @describeIn msgfPar Get \code{\link[base]{system}} compliant function call
#' 
#' @return \code{getMSGFpar}: A stringified version of the parameters compliant
#' with MS-GF+.
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
setMethod(
	'getMSGFpar', 'msgfPar',
	function(object){
        if(length(object) == 0) stop('Cannot get parameters for an empty msgfPar object')
        
		par <- list(object@tolerance, object@isotopeError, object@tda, object@fragmentation, object@instrument, object@enzyme, object@protocol, object@ntt, object@modification, object@lengthRange, object@chargeRange, object@matches)
		par <- sapply(par, getMSGFpar)
        par <- par[par!='']
		if(Sys.info()["sysname"] == 'Windows'){
			db <- paste('\"', object@database, '\"', sep='')
		} else {
			db <- gsub(' ', '\\ ', object@database, fixed=TRUE)
		}
		ans <- paste('-d', db, paste(par, collapse=' '))
		ans
	}
)

#' @describeIn msgfPar Initiate an MS-GF+ analysis using the selected msgfPar 
#' object
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
#' @return \code{runMSGF}: If \code{import=TRUE} an mzID or mzIDCollection 
#' object. If \code{async=TRUE} an msgfAsync object. Otherwise NULL
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
#' @importFrom mzID mzID
#' 
setMethod(
    'runMSGF', 'msgfPar',
    function(object, rawfiles, savenames, import=TRUE, memory=10000, async=FALSE, msgfPath){
        if(!file.exists(db(object))) stop('No database at specified location')
        if(!all(file.exists(rawfiles))) stop('Missing raw files at specified location')
        if(missing(import)) {
            import <- TRUE
        } else {}
        if(missing(memory)) {
            memory=10000
        } else {}
        if(missing(async)) {
            async=FALSE
        } else {}
        if(missing(msgfPath)) {
            if(!file.exists(file.path(system.file(package='MSGFplus'), 'MSGFPlus', 'MSGFPlus.jar'))) {
                message('First time using MSGFplus: Downloading MS-GF+ code')
                getMSGFplus()
            }
            msgfPath <- system.file(package='MSGFplus', 'MSGFPlus', 'MSGFPlus.jar')
        } else {
            if(!file.exists(msgfPath)) stop('No jar file at specified location')
        }
        if(Sys.info()["sysname"] == 'Windows'){
            msgfPath <- paste0('\"', msgfPath, '\"')
        }
        if(!missing(savenames) && length(rawfiles) != length(savenames)){
            stop('Number of raw files must correspond to number of savenames')
        } else {}
        if(missing(savenames)){
            savenames <- paste(sapply(strsplit(rawfiles,"\\."), function(x) paste(x[1:(length(x)-1)], collapse=".")), '.mzid', sep='')
        } else{}
        
        parameterCall <- getMSGFpar(object)
        
        if(async) {
            if(length(rawfiles) > 1) warning('Multiple rawfiles. Only the first will be used when running with "async=TRUE"')
            
            if(basename(savenames[1]) == savenames[1]){
                savenames[1] <- file.path(getwd(), savenames[1])
            }
            fileCall <- createFileCall(rawfiles[1], savenames[1])
            systemCall <- paste0('java -Xmx', memory, 'M -jar ', msgfPath, ' ', fileCall, ' ', parameterCall)
            checkfile <- tempfile('checkfile', fileext='.txt')
            system(paste0(systemCall, ' && echo "">', checkfile), wait=FALSE, ignore.stdout=TRUE, ignore.stderr=TRUE)
            return(new('msgfAsync', checkfile, savenames[1]))
        }
        
        for(i in 1:length(rawfiles)){
            if(basename(savenames[i]) == savenames[i]){
                savenames[i] <- file.path(getwd(), savenames[i])
            }
            fileCall <- createFileCall(rawfiles[i], savenames[i])
            systemCall <- paste0('java -Xmx', memory, 'M -jar ', msgfPath, ' ', fileCall, ' ', parameterCall)
            cat(systemCall, '\n\n')
            system(systemCall)
        }
        if(import){
            for(i in 1:length(savenames)){
                ans <- mzID(savenames)
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
#' Please consult the MS-GF+ documentation for full description of the 
#' parameters
#' 
#' @param database The location of the fasta file to use as search database
#' 
#' @param tolerance The parent ion tolerance to use. In simple cases a string in
#' the form '20 ppm' or '1 Da' or an msgfParTolerance object if asymmetric 
#' tolerance is desired
#' 
#' @param isotopeError The range of isotope errors used to correct for 
#' non-monoisotopic peaks. Either a numeric vector of length 2 specifying the 
#' lower and upper bounds of the range, or an msgfParIsotopeError object
#' 
#' @param tda \code{Logical} Should Target-Decoy approach be used to calculate 
#' FDR values.
#' 
#' @param fragmentation An integer specifying which fragmentation has been used 
#' during data acquisition. See details.
#' 
#' @param instrument An integer specifying the type of instrument used during 
#' data acquisition. See details.
#' 
#' @param enzyme An integer or name specifying the enzyme that has been used 
#' for protein digestion. See details.
#' 
#' @param protocol An integer or name specifying the type of preparation that 
#' has been done for the samples. See details.
#' 
#' @param ntt An integer specifying the cleavage specificity (Number of 
#' Tolerable Termini). 2 only allows fully tryptic peptide (if trypsin is used), 
#' 1 allows semitryptic peptides and 0 allows unspecific peptides
#' 
#' @param modification An msgfParModificationList object or a list containing 
#' the named elements nMod and modifications containing respectively an integer 
#' with the number of allowed modifications per petide and the modifications to 
#' search for as msgfParModification 
#' 
#' @param lengthRange A two element vector containing the lower and upper bounds 
#' of the residue length to search for
#' 
#' @param chargeRange A two element vector containing the lower and upper bounds 
#' of the charge range to search for
#' 
#' @param matches The number of matches to report per spectrum
#' 
#' @return An msgfPar object
#' 
#' @details
#' Fragmentation is usually specified as an integer according to the following 
#' lookup
#' \describe{
#'   \item{0}{As written in the spectrum or CID if no info}
#'   \item{1}{CID}
#'   \item{2}{ETD}
#'   \item{3}{HCD}
#'   \item{4}{Merge spectra from the same precursor}
#' }
#' It is possible to use the full name of the description for a more litteral 
#' function call
#' 
#' Instrument can likewise be specified as an integer or as a name according to 
#' this list
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
#' The protocol informs MS-GF+ whether a special sample treatment has been 
#' performed as part of the analysis. The protocol is specified according to 
#' the following list
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
#' @examples
#' # Example of specifying all parameters - usually not necessary
#' parameters <- msgfPar(
#'                       database=system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'),
#'                       tolerance='20 ppm',
#'                       isotopeError=c(0, 2),
#'                       tda=TRUE,
#'                       fragmentation='CID',
#'                       instrument='TOF',
#'                       enzyme='Lys-C',
#'                       protocol='No protocol',
#'                       ntt=2,
#'                       modification=list(
#'                           nMod=2,
#'                           modifications=list(
#'                               list(name='Carbamidomethyl',
#'                                    composition='C2H3N1O1',
#'                                    residues='C',
#'                                    type='fix',
#'                                    position='any'
#'                               ),
#'                               list(name='Oxidation',
#'                                    mass=15.994915,
#'                                    residues='M',
#'                                    type='opt',
#'                                    position='any'
#'                               )
#'                           )
#'                       ),
#'                       lengthRange=c(6,40),
#'                       chargeRange=c(2,7),
#'                       matches=1
#'                      )
#' parameters
#' 
#' @export
#' 
msgfPar <- function(database, tolerance, isotopeError, tda, fragmentation, instrument, enzyme, protocol, ntt, modification, lengthRange, chargeRange, matches){
	if(missing(database)) database <- ''
    par <- new('msgfPar', database=database)
	if(!missing(tolerance)){
        if(class(tolerance) == 'list') {
            tolerance <- do.call('msgfParTolerance', tolerance)
        }
        tolerance(par) <- tolerance
	} else {}
	if(!missing(isotopeError)){
	    if(class(isotopeError) == 'list') {
	        isotopeError <- do.call('msgfParIsotopeError', isotopeError)
	    }
	    isotopeError(par) <- isotopeError
	} else {}
	if(!missing(tda)){
	    if(class(tda) == 'list') {
	        tda <- do.call('msgfParTda', tda)
	    }
	    tda(par) <- tda
	} else {}
	if(!missing(fragmentation)){
	    if(class(fragmentation) == 'list') {
	        fragmentation <- do.call('msgfParFragmentation', fragmentation)
	    }
	    fragmentation(par) <- fragmentation
	} else {}
	if(!missing(instrument)){
	    if(class(instrument) == 'list') {
	        instrument <- do.call('msgfParInstrument', instrument)
	    }
	    instrument(par) <- instrument
	} else {}
	if(!missing(enzyme)){
	    if(class(enzyme) == 'list') {
	        enzyme <- do.call('msgfParEnzyme', enzyme)
	    }
	    enzyme(par) <- enzyme
	} else {}
	if(!missing(protocol)){
	    if(class(protocol) == 'list') {
	        protocol <- do.call('msgfParProtocol', protocol)
	    }
	    protocol(par) <- protocol
	} else {}
	if(!missing(ntt)){
	    if(class(ntt) == 'list') {
	        ntt <- do.call('msgfParNtt', ntt)
	    }
	    ntt(par) <- ntt
	} else {}
	if(!missing(modification)){
	    if(class(modification) == 'list') {
	        modification <- do.call('msgfParModificationList', modification)
	    }
	    mods(par) <- modification
	} else {}
	if(!missing(lengthRange)){
	    if(class(lengthRange) == 'list') {
	        lengthRange <- do.call('msgfParLengthRange', lengthRange)
	    }
	    lengthRange(par) <- lengthRange
	} else {}
	if(!missing(chargeRange)){
	    if(class(chargeRange) == 'list') {
	        chargeRange <- do.call('msgfParLengthRange', chargeRange)
	    }
	    chargeRange(par) <- chargeRange
	} else {}
	if(!missing(matches)){
	    if(class(matches) == 'list') {
	        matches <- do.call('msgfParLengthRange', matches)
	    }
	    matches(par) <- matches
	} else {}
    par
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
#' @seealso \code{\link{msgfPar-class}} \code{\link{msgfPar}}
#' 
#' @importFrom mzID mzIDparameters
#' 
#' @examples
#' \dontrun{
#' parameters <- msgfParFromID('result1.mzid')
#' }
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
    ans$isotopeError <- c(parameters@parameters$MinIsotopeError, parameters@parameters$MaxIsotopeError)
    ans$fragmentation <- parameters@parameters$FragmentMethod
    ans$instrument <- parameters@parameters$Instrument
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
    mod <- parameters@parameters$ModificationRules
    modifications <- sapply(1:nrow(mod), function(i) {
        try(msgfParModification(
            name=as.character(mod$name[i]), 
            mass=mod$massDelta[i], 
            residues=as.character(mod$residues[i]), 
            type=ifelse(mod$fixedMod[i], 'fix', 'opt'), 
            position=ifelse(tolower(mod$Specificity[i]) == 'any', 'any', 
                            ifelse(tolower(mod$Specificity[i]) == 'modification specificity n-term', 'N-term',
                                   ifelse(tolower(mod$Specificity[i]) == 'modification specificity c-term', 'C-term',
                                          ifelse(tolower(mod$Specificity[i]) == 'modification specificity prot-n-term', 'Prot-n-term',
                                                 ifelse(tolower(mod$Specificity[i]) == 'modification specificity prot-c-term', 'Prot-c-term', NA)
                                                 )
                                          )
                                   )
                            )
            )
        )}
    )
    if (any(sapply(modifications, function(x) {inherits(x, 'try-error')}))) {
        warning('Some modification rules removed due to bad formating')
    }
    modifications <- modifications[sapply(modifications, function(x) {!inherits(x, 'try-error')})]
    if(is.null(parameters@parameters$MaxNumModifications)) {
        warning('nMod not written in result file - set to 2')
        nmod <- 2
    } else {
        nmod <- parameters@parameters$MaxNumModifications
    }
	ans$modification <- list(nMod=nmod, modifications=modifications)
	do.call('msgfPar', ans)
}