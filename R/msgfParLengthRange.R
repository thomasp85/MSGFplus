#' A class handling length ranges
#' 
#' This class defines a length range and provides methods to get correct system
#' call parameters.
#' 
#' @slot value A numeric vector of length 2 describing the upper and lower 
#' bounds of the length range
#' 
#' @examples
#' lengths <- msgfParLengthRange(c(6, 40))
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParLengthRange',
		representation=representation(
				value='numeric'
		),
		validity=function(object){
			if(length(object) != 0){
				if(length(object@value) != 2){
				    return('value must be of length 2')
				} else if(object@value[1] >= object@value[2]){
					return('lower bound must be less than upper bound')
				} else {
                    return(TRUE)
				}
			} else {
				return(TRUE)
			}
		},
		prototype=prototype(
				value=as.numeric(NA)
		)
)
#' @describeIn msgfParLengthRange Short summary of msgfParLengthRange object
#' 
#' @param object An msgfParLengthRange object
#' 
setMethod(
		'show', 'msgfParLengthRange',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParLengthRange object\n')
			} else {
				cat(object@value[1], '-', object@value[2], '\n')
			}
		}
)
#' @describeIn msgfParLengthRange Report the length of an msgfParLengthRange 
#' object
#' 
#' @param x An msgfParLengthRange object
#' 
#' @return For length() An integer.
#' 
setMethod(
		'length', 'msgfParLengthRange',
		function(x){
			if(is.na(x@value[1])){
				0
			} else {
				1
			}
		}
)
#' @describeIn msgfParLengthRange Get \code{\link[base]{system}} compliant 
#' function call
#' 
#' @return For getMSGFpar() A string.
#' 
setMethod(
		'getMSGFpar', 'msgfParLengthRange',
		function(object){
			if(length(object) == 0){
				''
			} else {
				paste('-minLength ', object@value[1], ' -maxLength ', object@value[2], sep='')
			}
		}
)
#' @rdname msgfParLengthRange-class
#' 
#' @param value A numeric vector of length 2. The first element must be smaller 
#' than the last
#' 
#' @return For msgfParLengthRange() An msgfParLengthRange object.
#' 
#' @export
#' 
msgfParLengthRange <- function(value){
	if(missing(value)){
		new(Class='msgfParLengthRange')
	} else {
		new(Class='msgfParLengthRange', value=value)
	}
}
