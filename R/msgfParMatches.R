#' A class handling number of matches
#' 
#' This class defines a number of matches and provides methods to get correct 
#' system
#' call parameters.
#' 
#' @slot value An integer giving the number of matches per spectrum reported by 
#' MS-GF+
#' 
#' @examples
#' matches <- msgfParMatches(5)
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParMatches',
		representation=representation(
				value='numeric'
		),
		validity=function(object){
			if(length(object@value) != 1){
			    return('value can only be of length 1')
			} else if(floor(object@value) != object@value && !is.na(object@value)){
				return('value must be integer')
			} else {
                return(TRUE)
			}
		},
		prototype=prototype(
				value=as.numeric(NA)
		)
)
#' @describeIn msgfParMatches Short summary of msgfParMatches object
#' 
#' @param object An msgfParMatches object
#' 
setMethod(
		'show', 'msgfParMatches',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParMatches object\n')
			} else {
				cat(object@value, '\n')
			}
		}
)
#' @describeIn msgfParMatches Report the length of an msgfParMatches object
#' 
#' @param x An msgfParMatches object
#' 
#' @return For length() An integer.
#' 
setMethod(
		'length', 'msgfParMatches',
		function(x){
			if(is.na(x@value)){
				0
			} else {
				1
			}
		}
)
#' @describeIn msgfParMatches Get \code{\link[base]{system}} compliant function 
#' call
#' 
#' @return For getMSGFpar() A string.
#' 
setMethod(
		'getMSGFpar', 'msgfParMatches',
		function(object){
			if(length(object) == 0){
				''
			} else {
				paste('-n ', object@value, sep='')
			}
		}
)
#' @rdname msgfParMatches-class
#' 
#' @param value An integer giving the number of matches that should be returned 
#' per spectrum
#' 
#' @return For msgfParMatches() An msgfParMatches object.
#' 
#' @export
#' 
msgfParMatches <- function(value){
	if(missing(value)){
		new(Class='msgfParMatches')
	} else {
		new(Class='msgfParMatches', value=value)
	}
}
