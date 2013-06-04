# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setClass(
		Class='msgfParMatches',
		representation=representation(
				value='numeric'
		),
		validity=function(object){
			if(length(object@value) == 1){
				return(TRUE)
			} else {
				stop('value can only be of length 1')
			}
		},
		prototype=prototype(
				value=as.numeric(NA)
		)
)
setMethod(
		'show', 'msgfParMatches',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParMatches object\n')
			} else {
				cat(object@value, '\n')
			}
		}
)
setMethod(
		'length', 'msgfParMatches',
		function(x){
			if(is.na(x@value)){
				0
			} else {
				1
			}
		}
)
setMethod(
		'getMSGFpar', 'msgfParMatches',
		function(object){
			if(length(object) == 0){
				''
			} else {
				paste('-n ', object@value, sep='')
			}
		}
)
msgfParMatches <- function(value){
	if(missing(value)){
		new(Class='msgfParMatches')
	} else {
		new(Class='msgfParMatches', value=value)
	}
}
