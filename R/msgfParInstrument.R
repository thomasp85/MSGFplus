# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setClass(
		Class='msgfParInstrument',
		representation=representation(
				instrument='numeric'
		),
		validity=function(object){
			if(object@instrument %in% instrumentLookup()$Index & length(object@instrument) == 1){
				return(TRUE)
			} else {
				stop('Unknown instrument')
			}
		},
		prototype=prototype(
				instrument=as.numeric(NA)
		)
)
instrumentLookup <- function(){
	data.frame(Index=0:3, Description=c('Low-res LCQ/LTQ', 'High-res LTQ', 'TOF', 'Q-Exactive'))
}
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
msgfParInstrument <- function(instrument){
	if(missing(instrument)){
		new(Class='msgfParInstrument')
	} else {
		if(length(instrument) != 1) stop('Method must be of length 1')
		if(is.numeric(instrument)){
			new(Class='msgfParInstrument', instrument=instrument)
		} else if(is.character(instrument)){
			if(instrument %in% instrumentLookup()$Description){
				instrument <- instrumentLookup()$Index[instrumentLookup()$Description == instrument]
				new(Class='msgfParInstrument', instrument=instrument)
			} else stop('Unknown Instrument')
		} else stop('method must be either numeric or string')
	}
}
