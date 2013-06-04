# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setClass(
		Class='msgfParTolerance',
		representation=representation(
				unit = 'character',
				low = 'numeric',
				high = 'numeric'
		), 
		validity = function(object){
			if(object@unit %in% c('ppm', 'Da')){
				TRUE
			} else {
				stop('Wrong unit parameter. Use \'ppm\' or \'Da\'')
			}
		},
		prototype=prototype(
				unit='ppm',
				low=as.numeric(NA),
				high=as.numeric(NA)
		)
)
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
msgfParTolerance <- function(value, low, high, unit){
	if(!missing(value)){
		low <- value
		high <- value
	} else if(any(missing(low), missing(high))){
		return(new(Class = 'msgfParTolerance'))
	} else {}
	new(Class = 'msgfParTolerance', unit=unit, low=low, high=high)
}
