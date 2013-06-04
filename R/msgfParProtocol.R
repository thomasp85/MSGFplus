# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setClass(
		Class='msgfParProtocol',
		representation=representation(
				protocol='numeric'
		),
		validity=function(object){
			if(object@protocol %in% protocolLookup()$Index & length(object@protocol) == 1){
				return(TRUE)
			} else {
				stop('Unknown protocol')
			}
		},
		prototype=prototype(
				protocol=as.numeric(NA)
		)
)
protocolLookup <- function(){
	data.frame(Index=0:3, Description=c('No protocol', 'Phosphorylation', 'iTRAQ', 'iTRAQPhospho'))
}
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
setMethod(
		'getMSGFpar', 'msgfParProtocol',
		function(object){
			if(length(object) == 0){
				''
			} else {
				paste('-p ', object@protocol, sep='')
			}
		}
)
msgfParProtocol <- function(protocol){
	if(missing(protocol)){
		new(Class='msgfParProtocol')
	} else {
		if(length(protocol) != 1) stop('protocol must be of length 1')
		if(is.numeric(protocol)){
			new(Class='msgfParProtocol', protocol=protocol)
		} else if(is.character(protocol)){
			if(protocol %in% protocolLookup()$Description){
				protocol <- protocolLookup()$Index[protocolLookup()$Description == protocol]
				new(Class='msgfParProtocol', protocol=protocol)
			} else stop('Unknown protocol')
		} else stop('protocol must be either numeric or string')		
	}
}
