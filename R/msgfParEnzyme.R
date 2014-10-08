#' A class handling enzyme selection
#' 
#' This class defines a digestion enzyme selection and provides methods to get 
#' correct system call parameters.
#' 
#' @slot enzyme An integer specifiying the selection of enzyme. See the detail 
#' section of \code{\link{msgfPar}}
#' 
#' @examples
#' enzyme <- msgfParEnzyme(1)
#' enzyme <- msgfParEnzyme('Trypsin')
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParEnzyme',
		representation=representation(
				enzyme='numeric'
		),
		validity=function(object){
			if(object@enzyme %in% enzymeLookup()$Index & length(object@enzyme) == 1){
				return(TRUE)
			} else {
				return('Unknown enzyme')
			}
		},
		prototype=prototype(
				enzyme=as.numeric(NA)
		)
)
enzymeLookup <- function(){
	data.frame(Index=0:9, Description=c('unspecific cleavage', 'Trypsin', 'Chymotrypsin', 'Lys-C', 'Lys-N', 'glutamyl endopeptidase (Glu-C)', 'Arg-C', 'Asp-N', 'alphaLP', 'no cleavage'))
}
#' @describeIn msgfParEnzyme Short summary of msgfParEnzyme object
#' 
#' @param object An msgfParEnzyme object
#' 
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
#' @describeIn msgfParEnzyme Report the length of an msgfParEnzyme object
#' 
#' @param x An msgfParEnzyme object
#' 
#' @return For length() An integer.
#' 
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
#' @describeIn msgfParEnzyme Get \code{\link[base]{system}} compliant function 
#' call
#' 
#' @return For getMSGFpar() A string.
#' 
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
#' @rdname msgfParEnzyme-class
#' 
#' @param enzyme Either an integer or a string
#' 
#' @return For msgfParEnzyme() An msgfParEnzyme object.
#' 
#' @export
#' 
msgfParEnzyme <- function(enzyme){
	if(missing(enzyme)){
		new(Class='msgfParEnzyme')
	} else {
		if(length(enzyme) != 1) stop('enzyme must be of length 1')
		if(is.numeric(enzyme)){
			new(Class='msgfParEnzyme', enzyme=enzyme)
		} else if(is.character(enzyme)){
			if(tolower(enzyme) %in% tolower(enzymeLookup()$Description)){
				enzyme <- enzymeLookup()$Index[tolower(enzymeLookup()$Description) == tolower(enzyme)]
				new(Class='msgfParEnzyme', enzyme=enzyme)
			} else stop('Unknown enzyme')
		} else stop('enzyme must be either numeric or string')
	}
}
