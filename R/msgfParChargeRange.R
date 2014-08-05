#' A class handling charge ranges
#' 
#' This class defines a charge range and provides methods to get correct system
#' call parameters.
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{value}:}{A numeric vector of length 2 describing the upper and lower bounds of the charge range}
#' }
#' 
#' @family msgfParClasses
#' 
setClass(
		Class='msgfParChargeRange',
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
		'show', 'msgfParChargeRange',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParChargeRange object\n')
			} else {
				cat(object@value[1], '-', object@value[2], '\n')
			}
		}
)
setMethod(
		'length', 'msgfParChargeRange',
		function(x){
			if(is.na(x@value[1])){
				0
			} else {
				1
			}
		}
)
#' see getMSGFpar-methods
#' 
#' @noRd
#' 
setMethod(
		'getMSGFpar', 'msgfParChargeRange',
		function(object){
			if(length(object) == 0){
				''
			} else {
				paste('-minCharge ', object@value[1], ' -maxCharge ', object@value[2], sep='')
			}
		}
)

#' @rdname msgfParChargeRange-class
#' 
#' @param value A numeric vector of length 2. The first element must be smaller than the last
#' 
#' @return An msgfParChargeRange object
#' 
#' @export
#' 
msgfParChargeRange <- function(value){
	if(missing(value)){
		new(Class='msgfParChargeRange')
	} else {
		new(Class='msgfParChargeRange', value=value)
	}
}
