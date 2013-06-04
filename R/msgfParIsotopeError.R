# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setClass(
		Class='msgfParIsotopeError',
		representation=representation(
				range='numeric'
		),
		validity=function(object){
			if(all(floor(object@range) == object@range)){
				return(TRUE)
			} else {
				stop('range must consist of integers')
			}
		},
		prototype=prototype(
				range=as.numeric(NA)
		)
)
setMethod(
		'show', 'msgfParIsotopeError',
		function(object){
			if(length(object) == 0){
				cat('An empty istopeError object\n')
			} else {
				cat(object@range, '\n')
			}
		}
)
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
setMethod(
		'getMSGFpar', 'msgfParIsotopeError',
		function(object){
			if(length(object) == 0){
				''
			} else {
				paste('-ti ', paste(object@range, collapse=','), sep='')
			}
		}
)
msgfParIsotopeError <- function(range){
	if(missing(range)){
		new(Class='msgfParIsotopeError')
	} else {
		new(Class='msgfParIsotopeError', range=range)
	}
}
