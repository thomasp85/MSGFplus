# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setClass(
		Class='msgfParEnzyme',
		representation=representation(
				enzyme='numeric'
		),
		validity=function(object){
			if(object@enzyme %in% enzymeLookup()$Index & length(object@enzyme) == 1){
				return(TRUE)
			} else {
				stop('Unknown enzyme')
			}
		},
		prototype=prototype(
				enzyme=as.numeric(NA)
		)
)
enzymeLookup <- function(){
	data.frame(Index=0:9, Description=c('Unspecific cleavage', 'Trypsin', 'Chymotrypsin', 'Lys-C', 'Lys-N', 'glutamyl endopeptidase (Glu-C)', 'Arg-C', 'Asp-N', 'alphaLP', 'No cleavage'))
}
setMethod(
		'show', 'msgfParEnzyme',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParEnzyme object\n')
			} else {
				desc <- enzymeLookup()$Description[enzymeLookup()$Index == object@enzyme]
				cat(paste(object@enzyme, ':\t', desc, '\n', sep=''))
			}
		}
)
setMethod(
		'length', 'msgfParEnzyme',
		function(x){
			if(is.na(x@enzyme)){
				0
			} else {
				1
			}
		}
)
setMethod(
		'getMSGFpar', 'msgfParEnzyme',
		function(object){
			if(length(object) == 0){
				''
			} else {
				paste('-e ', object@enzyme, sep='')
			}
		}
)
msgfParEnzyme <- function(enzyme){
	if(missing(enzyme)){
		new(Class='msgfParEnzyme')
	} else {
		if(length(enzyme) != 1) stop('enzyme must be of length 1')
		if(is.numeric(enzyme)){
			new(Class='msgfParEnzyme', enzyme=enzyme)
		} else if(is.character(enzyme)){
			if(enzyme %in% enzymeLookup()$Description){
				enzyme <- enzymeLookup()$Index[enzymeLookup()$Description == enzyme]
				new(Class='msgfParEnzyme', enzyme=enzyme)
			} else stop('Unknown Enzyme')
		} else stop('enzyme must be either numeric or string')
	}
}
