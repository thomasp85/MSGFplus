#' A class handling charge ranges
#' 
#' This class defines a charge range and provides methods to get correct system
#' call parameters.
#' 
#' @slot value A numeric vector of length 2 describing the upper and lower 
#' bounds of the charge range
#' 
#' @examples
#' charges <- msgfParChargeRange(c(2, 5))
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParChargeRange',
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
#' @describeIn msgfParChargeRange Short summary of msgfParChargeRange object
#' 
#' @param object An msgfParChargeRange object
#' 
setMethod(
		'show', 'msgfParChargeRange',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParChargeRange object\n')
			} else {
				cat(object@value[1], '-', object@value[2], '\n')
			}
		}
)
#' @describeIn msgfParChargeRange Report the length of an msgfParChargeRange 
#' object
#' 
#' @param x An msgfParChargeRange object
#' 
#' @return For length() An integer.
#' 
setMethod(
		'length', 'msgfParChargeRange',
		function(x){
			if(is.na(x@value[1])){
				0
			} else {
				1
			}
		}
)
#' @describeIn msgfParChargeRange Get \code{\link[base]{system}} compliant 
#' function call
#' 
#' @return For getMSGFpar() A string.
#' 
setMethod(
		'getMSGFpar', 'msgfParChargeRange',
		function(object){
			if(length(object) == 0){
				''
			} else {
				paste('-minCharge ', object@value[1], ' -maxCharge ', object@value[2], sep='')
			}
		}
)
#' @rdname msgfParChargeRange-class
#' 
#' @param value A numeric vector of length 2. The first element must be smaller 
#' than the last
#' 
#' @return For msgfParChargeRange() An msgfParChargeRange object.
#' 
#' @export
#' 
msgfParChargeRange <- function(value){
	if(missing(value)){
		new(Class='msgfParChargeRange')
	} else {
		new(Class='msgfParChargeRange', value=value)
	}
}
