#' A class handling instrument types
#' 
#' This class defines an instrument type and provides methods to get correct system
#' call parameters.
#' 
#' @slot instrument An integer specifying the instrument type
#' 
#' @examples
#' instrument <- msgfParInstrument(1)
#' instrument <- msgfParInstrument('HighRes')
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParInstrument',
		representation=representation(
				instrument='numeric'
		),
		validity=function(object){
			if(object@instrument %in% instrumentLookup()$Index & length(object@instrument) == 1){
				return(TRUE)
			} else {
				return('Unknown instrument')
			}
		},
		prototype=prototype(
				instrument=as.numeric(NA)
		)
)
instrumentLookup <- function(){
	data.frame(Index=0:3, Description=c('LowRes', 'HighRes', 'TOF', 'QExactive'))
}
#' @describeIn msgfParInstrument Short summary of msgfParInstrument object
#' 
#' @param object An msgfParInstrument object
#' 
setMethod(
		'show', 'msgfParInstrument',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParInstrument object\n')
			} else {
				desc <- instrumentLookup()$Description[instrumentLookup()$Index == object@instrument]
				cat(paste(object@instrument, ':\t', desc, '\n', sep=''))
			}
		}
)
#' @describeIn msgfParInstrument Report the length of an msgfParInstrument object
#' 
#' @param x An msgfParInstrument object
#' 
#' @return For length() An integer.
#' 
setMethod(
		'length', 'msgfParInstrument',
		function(x){
			if(is.na(x@instrument)){
				0
			} else {
				1
			}
		}
)
#' @describeIn msgfParInstrument Get \code{\link[base]{system}} compliant function call
#' 
#' @return For getMSGFpar() A string.
#' 
setMethod(
		'getMSGFpar', 'msgfParInstrument',
		function(object){
			if(length(object) == 0){
				''
			} else {
				paste('-inst ', object@instrument, sep='')
			}
		}
)
#' @rdname msgfParInstrument-class
#' 
#' @param instrument An integer specifying the instrument type
#' 
#' @return For msgfParInstrument() An msgfParInstrument object.
#' 
#' @export
#' 
msgfParInstrument <- function(instrument){
	if(missing(instrument)){
		new(Class='msgfParInstrument')
	} else {
		if(length(instrument) != 1) stop('Method must be of length 1')
		if(is.numeric(instrument)){
			new(Class='msgfParInstrument', instrument=instrument)
		} else if(is.character(instrument)){
			if(tolower(instrument) %in% tolower(instrumentLookup()$Description)){
				instrument <- instrumentLookup()$Index[tolower(instrumentLookup()$Description) == tolower(instrument)]
				new(Class='msgfParInstrument', instrument=instrument)
			} else stop('Unknown instrument')
		} else stop('method must be either numeric or string')
	}
}
