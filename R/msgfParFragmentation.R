# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setClass(
		Class='msgfParFragmentation',
		representation=representation(
				method='numeric'
		),
		validity=function(object){
			if((object@method %in% fragmentationLookup()$Index | is.na(object@method)) & length(object@method) == 1){
				return(TRUE)
			} else {
				stop('Unknown fragmentation method')
			}
		},
		prototype=prototype(
				method=as.numeric(NA)
		)
)
fragmentationLookup <- function(){
	data.frame(Index=0:4, Description=c('As written in the spectrum or CID if no info', 'CID', 'ETD', 'HCD', 'Merge spectra from the same precursor'))
}
setMethod(
		'show', 'msgfParFragmentation',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParFragmentation object\n')
			} else {
				desc <- fragmentationLookup()$Description[fragmentationLookup()$Index == object@method]
				cat(paste(object@method, ':\t', desc, '\n', sep=''))
			}
		}
)
setMethod(
		'length', 'msgfParFragmentation',
		function(x){
			if(is.na(x@method)){
				0
			} else {
				1
			}
		}
)
setMethod(
		'getMSGFpar', 'msgfParFragmentation',
		function(object){
			if(length(object)==0){
				''
			} else {
				paste('-m ', object@method, sep='')
			}
		}
)
msgfParFragmentation <- function(method){
	if(missing(method)){
		new(Class='msgfParFragmentation')
	} else {
		if(length(method) != 1) stop('Method must be of length 1')
		if(is.numeric(method)){
			new(Class='msgfParFragmentation', method=method)
		} else if(is.character(method)){
			if(tolower(method) %in% tolower(fragmentationLookup()$Description)){
				method <- fragmentationLookup()$Index[tolower(fragmentationLookup()$Description) == tolower(method)]
				new(Class='msgfParFragmentation', method=method)
			} else stop('Unknown fragmentation method')
		} else stop('method must be either numeric or string')
	}
}
