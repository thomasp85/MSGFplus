#' A class handling cleavage specificity
#' 
#' This class defines cleavage specificity and provides methods to get correct 
#' system call parameters.
#' 
#' @slot value An integer between 0 and 2 that specifies the specificity (2: 
#' full cleavage, 1: semi specific cleavage, 0: no specificity)
#' 
#' @examples
#' ntt <- msgfParNtt(2)
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParNtt',
		representation=representation(
				value='numeric'
		),
		validity=function(object){
			if(length(object@value) != 1){
			    return('value can only be of length 1')
			} else if(!object@value %in% 0:2){
				return('value must be between 0 and 2')
			} else {
                return(TRUE)
			}
		},
		prototype=prototype(
				value=as.numeric(NA)
		)
)
#' @describeIn msgfParNtt Short summary of msgfParNtt object
#' 
#' @param object An msgfParNtt object
#' 
setMethod(
		'show', 'msgfParNtt',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParNtt object\n')
			} else {
				cat(object@value, '\n')
			}
		}
)
#' @describeIn msgfParNtt Report the length of an msgfParNtt object
#' 
#' @param x An msgfParNtt object
#' 
#' @return For length() An integer.
#' 
setMethod(
		'length', 'msgfParNtt',
		function(x){
			if(is.na(x@value)){
				0
			} else {
				1
			}
		}
)
#' @describeIn msgfParNtt Get \code{\link[base]{system}} compliant function call
#' 
#' @return For getMSGFpar() A string.
#' 
setMethod(
		'getMSGFpar', 'msgfParNtt',
		function(object){
			if(length(object) == 0){
				''
			} else {
				paste('-ntt ', object@value, sep='')
			}
		}
)
#' @rdname msgfParNtt-class
#' 
#' @param value An integer between 0 and 2 that specifies the specificity (2: 
#' full cleavage, 1: semi specific cleavage, 0: no specificity)
#' 
#' @return For msgfParNtt() An msgfParNtt object.
#' 
#' @export
#' 
msgfParNtt <- function(value){
	if(missing(value)){
		new(Class='msgfParNtt')
	} else {
		new(Class='msgfParNtt', value=value)
	}
}
