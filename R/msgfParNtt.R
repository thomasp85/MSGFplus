# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setClass(
		Class='msgfParNtt',
		representation=representation(
				value='numeric'
		),
		validity=function(object){
			if(length(object@value) == 1 & object@value %in% 0:2){
				return(TRUE)
			} else {
				stop('value can only be of length 1 and between 0 and 2')
			}
		},
		prototype=prototype(
				value=as.numeric(NA)
		)
)
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
msgfParNtt <- function(value){
	if(missing(value)){
		new(Class='msgfParNtt')
	} else {
		new(Class='msgfParNtt', value=value)
	}
}
