#' A class handling protocol choice
#' 
#' This class defines a protocol and provides methods to get correct system
#' call parameters.
#' 
#' @slot protocol An integer specifying a specific protocol type
#' 
#' @examples
#' protocol <- msgfParProtocol(0)
#' protocol <- msgfParProtocol('No protocol')
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParProtocol',
		representation=representation(
				protocol='numeric'
		),
		validity=function(object){
			if(object@protocol %in% protocolLookup()$Index & length(object@protocol) == 1){
				return(TRUE)
			} else {
				return('Unknown protocol')
			}
		},
		prototype=prototype(
				protocol=as.numeric(NA)
		)
)
protocolLookup <- function(){
	data.frame(Index=0:3, Description=c('No protocol', 'Phosphorylation', 'iTRAQ', 'iTRAQPhospho'))
}
#' @describeIn msgfParProtocol Short summary of msgfParProtocol object
#' 
#' @param object An msgfParProtocol object
#' 
setMethod(
		'show', 'msgfParProtocol',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParProtocol object')
			} else {
				desc <- protocolLookup()$Description[protocolLookup()$Index == object@protocol]
				cat(paste(object@protocol, ':\t', desc, '\n', sep=''))
			}
		}
)
#' @describeIn msgfParProtocol Report the length of an msgfParProtocol object
#' 
#' @param x An msgfParProtocol object
#' 
#' @return For length() An integer.
#' 
setMethod(
		'length', 'msgfParProtocol',
		function(x){
			if(is.na(x@protocol)){
				0
			} else {
				1
			}
		}
)
#' @describeIn msgfParProtocol Get \code{\link[base]{system}} compliant function call
#' 
#' @return For getMSGFpar() A string.
#' 
setMethod(
		'getMSGFpar', 'msgfParProtocol',
		function(object){
			if(length(object) == 0){
				''
			} else {
				paste('-protocol ', object@protocol, sep='')
			}
		}
)
#' @rdname msgfParProtocol-class
#' 
#' @param protocol An integer or string specifying the protocol to use.
#' 
#' @return For msgfParProtocol() An msgfParProtocol object.
#' 
#' @export
#' 
msgfParProtocol <- function(protocol){
	if(missing(protocol)){
		new(Class='msgfParProtocol')
	} else {
		if(length(protocol) != 1) stop('protocol must be of length 1')
		if(is.numeric(protocol)){
			new(Class='msgfParProtocol', protocol=protocol)
		} else if(is.character(protocol)){
			if(tolower(protocol) %in% tolower(protocolLookup()$Description)){
				protocol <- protocolLookup()$Index[tolower(protocolLookup()$Description) == tolower(protocol)]
				new(Class='msgfParProtocol', protocol=protocol)
			} else stop('Unknown protocol')
		} else stop('protocol must be either numeric or string')		
	}
}
