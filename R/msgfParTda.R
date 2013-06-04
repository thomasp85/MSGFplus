# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setClass(
		Class='msgfParTda',
		representation=representation(
				tda='logical'
		),
		validity=function(object){
			if(length(object@tda) == 1){
				return(TRUE)
			} else {
				stop('tda can only be of length 1')
			}
		},
		prototype=prototype(
				tda=as.logical(NA)
		)
)
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
msgfParTda <- function(value){
	if(missing(value)){
		new(Class='msgfParTda')
	} else {
		new(Class='msgfParTda', tda=value)
	}
}
