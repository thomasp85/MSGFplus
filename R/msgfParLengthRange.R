# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setClass(
		Class='msgfParLengthRange',
		representation=representation(
				value='numeric'
		),
		validity=function(object){
			if(length(object) != 0){
				if((length(object@value) == 2 & object@value[1] <= object@value[2])){
					return(TRUE)
				} else {
					stop('value must be of length 2')
				}
			} else {
				return(TRUE)
			}
		},
		prototype=prototype(
				value=as.numeric(NA)
		)
)
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
msgfParLengthRange <- function(value){
	if(missing(value)){
		new(Class='msgfParLengthRange')
	} else {
		new(Class='msgfParLengthRange', value=value)
	}
}
