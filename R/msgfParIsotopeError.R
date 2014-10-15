#' A class handling isotope errors
#' 
#' This class defines a set of isotopes that should be included for error 
#' correction and provides methods to get correct system call parameters.
#' 
#' @slot range An integer vector with lower and upper bounds of isotopes to 
#' error correct
#' 
#' @examples
#' isotopeError <- msgfParIsotopeError(c(0, 2))
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParIsotopeError',
		representation=representation(
				range='numeric'
		),
		validity=function(object){
			if(all(floor(object@range) != object@range)){
				return('range must consist of integers')
			}
            if(length(object@range) != 2) {
                return('Range must have length 2')
            }
			return(TRUE)
		},
		prototype=prototype(
				range=as.numeric(NA)
		)
)
#' @describeIn msgfParIsotopeError Short summary of msgfParIsotopeError object
#' 
#' @param object An msgfParIsotopeError object
#' 
setMethod(
		'show', 'msgfParIsotopeError',
		function(object){
			if(length(object) == 0){
				cat('An empty istopeError object\n')
			} else {
			    cat(object@range[1], '-', object@range[2], '\n')
			}
		}
)
#' @describeIn msgfParIsotopeError Report the length of an msgfParIsotopeError 
#' object
#' 
#' @param x An msgfParIsotopeError object
#' 
#' @return For length() An integer.
#' 
setMethod(
		'length', 'msgfParIsotopeError',
		function(x){
			if(is.na(x@range[1])){
				0
			} else {
				1
			}
		}
)
#' @describeIn msgfParIsotopeError Get \code{\link[base]{system}} compliant 
#' function call
#' 
#' @return For getMSGFpar() A string.
#' 
setMethod(
		'getMSGFpar', 'msgfParIsotopeError',
		function(object){
			if(length(object) == 0){
				''
			} else {
                range <- paste(object@range, collapse=',')
                if(Sys.info()["sysname"] == 'Windows'){
                    range <- paste0('\"', range, '\"')
                }
				paste0('-ti ', range)
			}
		}
)
#' @rdname msgfParIsotopeError-class
#' 
#' @param range An integer vector with isotopes
#' 
#' @return For msgfParIsotopeError() An msgfParIsotopeError object.
#' 
#' @export
#' 
msgfParIsotopeError <- function(range){
	if(missing(range)){
		new(Class='msgfParIsotopeError')
	} else {
		new(Class='msgfParIsotopeError', range=range)
	}
}
