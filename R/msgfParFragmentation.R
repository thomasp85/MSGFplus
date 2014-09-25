#' A class handling Fragmentation types
#' 
#' This class defines a fragmentation type and provides methods to get correct system
#' call parameters.
#' 
#' @slot method An integer between 0 and 4 giving the selected method
#' 
#' @examples
#' fragmentation <- msgfParFragmentation(1)
#' fragmentation <- msgfParFragmentation('CID')
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParFragmentation',
		representation=representation(
				method='numeric'
		),
		validity=function(object){
			if((object@method %in% fragmentationLookup()$Index | is.na(object@method)) & length(object@method) == 1){
				return(TRUE)
			} else {
				return('Unknown fragmentation method')
			}
		},
		prototype=prototype(
				method=as.numeric(NA)
		)
)
fragmentationLookup <- function(){
	data.frame(Index=0:4, Description=c('As written in the spectrum or CID if no info', 'CID', 'ETD', 'HCD', 'Merge spectra from the same precursor'))
}
#' @describeIn msgfParFragmentation Short summary of msgfParFragmentation object
#' 
#' @param object An msgfParFragmentation object
#' 
setMethod(
		'show', 'msgfParFragmentation',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParFragmentation object\n')
			} else {
				desc <- fragmentationLookup()$Description[fragmentationLookup()$Index == object@method]
				cat(paste(object@method, ':\t', desc, '\n', sep=''))
			}
		}
)
#' @describeIn msgfParFragmentation Report the length of an msgfParFragmentation object
#' 
#' @param x An msgfParFragmentation object
#' 
#' @return For length() An integer.
#' 
setMethod(
		'length', 'msgfParFragmentation',
		function(x){
			if(is.na(x@method)){
				0
			} else {
				1
			}
		}
)
#' @describeIn msgfParFragmentation Get \code{\link[base]{system}} compliant function call
#' 
#' @return For getMSGFpar() A string.
#' 
setMethod(
		'getMSGFpar', 'msgfParFragmentation',
		function(object){
			if(length(object)==0){
				''
			} else {
				paste('-m ', object@method, sep='')
			}
		}
)
#' @rdname msgfParFragmentation-class
#' 
#' @param method An integer specifying the method
#' 
#' @return For msgfParFragmentation() An msgfParFragmentation object.
#' 
#' @export
#' 
msgfParFragmentation <- function(method){
	if(missing(method)){
		new(Class='msgfParFragmentation')
	} else {
		if(length(method) != 1) stop('Method must be of length 1')
		if(is.numeric(method)){
			new(Class='msgfParFragmentation', method=method)
		} else if(is.character(method)){
			if(tolower(method) %in% tolower(fragmentationLookup()$Description)){
				method <- fragmentationLookup()$Index[tolower(fragmentationLookup()$Description) == tolower(method)]
				new(Class='msgfParFragmentation', method=method)
			} else stop('Unknown fragmentation method')
		} else stop('method must be either numeric or string')
	}
}
