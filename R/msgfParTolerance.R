#' A class handling parent ion tolerance
#' 
#' This class defines a parent ion tolerance and provides methods to get correct 
#' system call parameters.
#' 
#' @slot unit The unit used to define the tolerance
#' @slot low The lower bound of the tolerance
#' @slot high The higher bound of the tolerance
#' 
#' @examples
#' # Symmetric
#' tolerance <- msgfParTolerance(20, unit='ppm')
#' 
#' # Asymmetric
#' tolerance <- msgfParTolerance(low=0.5, high=1.5, unit='Da')
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParTolerance',
		representation=representation(
				unit = 'character',
				low = 'numeric',
				high = 'numeric'
		), 
		validity = function(object){
			if(!object@unit %in% c('ppm', 'Da')){
			    return('Wrong unit parameter. Use \'ppm\' or \'Da\'')
			} else if(length(object@low) != 1 || length(object@high) != 1){
				return('low and high must have length 1')
			} else {
                return(TRUE)
			}
		},
		prototype=prototype(
				unit='ppm',
				low=as.numeric(NA),
				high=as.numeric(NA)
		)
)
#' @describeIn msgfParTolerance Short summary of msgfParTolerance object
#' 
#' @param object An msgfParTolerance object
#' 
setMethod(
		'show', 'msgfParTolerance',
		function(object){
			if(any(is.na(object@low), is.na(object@high))){
				cat('An empty msgfParTolerance object\n')
			} else {
				if(object@low == object@high){
					cat(paste(object@low, object@unit, '\n', sep=' '))
				} else {
					cat(paste(object@low, object@unit, '-', object@high, object@unit, '\n', sep=' '))
				}
			}
		}
)
#' @describeIn msgfParTolerance Report the length of an msgfParTolerance object
#' 
#' @param x An msgfParTolerance object
#' 
#' @return For length() An integer.
#' 
setMethod(
		'length', 'msgfParTolerance',
		function(x){
			if(any(is.na(x@low), is.na(x@high))){
				0
			} else {
				1
			}
		}
)
#' @describeIn msgfParTolerance Get \code{\link[base]{system}} compliant 
#' function call
#' 
#' @return For getMSGFpar() A string.
#' 
setMethod(
		'getMSGFpar', 'msgfParTolerance',
		function(object){
			if(length(object) == 0){
				''
			} else {
				if(object@low == object@high){
					paste('-t ', object@low, object@unit, sep='')
				} else {
					paste('-t ', object@low, object@unit, ',', object@high, object@unit, sep='')
				}
			}
		}
)
#' @rdname msgfParTolerance-class
#' 
#' @param value A numeric giving the upper and lower bounds of the tolerance
#' 
#' @param low A numeric giving the lower bounds of the tolerance
#' 
#' @param high A numeric giving the higher bounds of the tolerance
#' 
#' @param unit The unit used. Either 'ppm' or 'Da'
#' 
#' @return For msgfParTolerance() An msgfParTolerance object.
#' 
#' @export
#' 
msgfParTolerance <- function(value, low, high, unit){
	if(!missing(value)){
		low <- value[1]
		high <- ifelse(length(value) == 1, value[1], value[2])
	} else if(any(missing(low), missing(high))){
		return(new(Class = 'msgfParTolerance'))
	} else {}
	new(Class = 'msgfParTolerance', unit=unit, low=low, high=high)
}
