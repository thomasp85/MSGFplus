#' A class handling use of target-decoy approach for FDR estimation
#' 
#' This class defines whether to use target-decoy approach and provides methods 
#' to get correct system call parameters.
#' 
#' @slot tda A boolean defining whether to use tda or not
#' 
#' @examples
#' tda <- msgfParTda(TRUE)
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParTda',
		representation=representation(
				tda='logical'
		),
		validity=function(object){
			if(length(object@tda) == 1){
				return(TRUE)
			} else {
				return('tda can only be of length 1')
			}
		},
		prototype=prototype(
				tda=as.logical(NA)
		)
)
#' @describeIn msgfParTda Short summary of msgfParTda object
#' 
#' @param object An msgfParTda object
#' 
setMethod(
		'show', 'msgfParTda',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParTda object\n')
			} else {
				cat(object@tda, '\n')
			}
		}
)
#' @describeIn msgfParTda Report the length of an msgfParTda object
#' 
#' @param x An msgfParTda object
#' 
#' @return For length() An integer.
#' 
setMethod(
		'length', 'msgfParTda',
		function(x){
			if(is.na(x@tda)){
				0
			} else {
				1
			}
		}
)
#' @describeIn msgfParTda Get \code{\link[base]{system}} compliant function call
#' 
#' @return For getMSGFpar() A string.
#' 
setMethod(
		'getMSGFpar', 'msgfParTda',
		function(object){
			if(length(object) == 0){
				''
			} else if(object@tda){
				'-tda 1'
			} else {
				'-tda 0'
			}
		}
)
#' @rdname msgfParTda-class
#' 
#' @param value A boolean defining whether to use tda or not
#' 
#' @return For msgfParTda() An msgfParTda object.
#' 
#' @export
#' 
msgfParTda <- function(value){
	if(missing(value)){
		new(Class='msgfParTda')
	} else {
		new(Class='msgfParTda', tda=value)
	}
}
