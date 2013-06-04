# TODO: Add comment
# 
# Author: Thomas
###############################################################################

setClassUnion('mzData', c('NULL', 'mzRramp', 'mzRnetCDF'))
setClass(
	Class='msgfRaw',
	representation=representation(
			location = 'character',
			connection = 'mzData'
	),
	validity = function(object){
		if(length(object) != 0){
			if(!file.exists(object@location)){
				stop('File does not exist')
			} else {
				return(TRUE)
			}
		} else {
			return(TRUE)
		}
		
	},
	prototype=prototype(
		location='',
		connection=NULL
	)
)

setMethod(
	'length', 'msgfRaw',
	function(x){
		if(x@location == ''){
			0
		} else {
			1
		}
	}
)
setMethod(
		'show', 'msgfRaw',
		function(object){
			if(length(object) != 0){
				cat(object@location)
				cat('\n\n')
				cat('Loaded: ', isLoaded(object), '\n', sep='')
				if(isLoaded(object)){
					cat('\n')
					show(object@connection)
				}
			} else {
				cat('An empty msgfRaw object\n')
			}
		}
)
setMethod(
	'load', 'msgfRaw',
	function(file, envir=parent.frame()){
		if(!isLoaded(file)){
			if(length(file) != 0){
				file@connection <- openMSfile(file@location)
			} else {
				warning('Can\'t load an empty msgfRaw object')
			}
		}
		file
	}
)
setMethod(
	'isLoaded', 'msgfRaw',
	function(object){
		if(is.null(object@connection)){
			FALSE
		} else {
			TRUE
		}
	}
)
setMethod(
	'fileName', 'msgfRaw',
	function(object){
		object@location
	}
)
setMethod(
	'analyzer', 'msgfRaw',
	function(object){
		if(isLoaded(object)) analyzer(object@connection)
	}
)
setMethod(
		'close', 'msgfRaw',
		function(con){
			if(isLoaded(object)){
				close(con@connection)
				con@connection <- NULL
				con
			}
		}
)
setMethod(
		'detector', 'msgfRaw',
		function(object){
			if(isLoaded(object)) detector(object@connection)
		}
)
setMethod(
		'get3Dmap', 'msgfRaw',
		function(object, scans, lowMz, highMz, resMz){
			if(isLoaded(object)) get3Dmap(object@connection, scans, lowMz, highMz, resMz)
		}
)
setMethod(
		'header', 'msgfRaw',
		function(object, ...){
			if(isLoaded(object)) header(object@connection, ...)
		}
)
setMethod(
		'instrumentInfo', 'msgfRaw',
		function(object){
			if(isLoaded(object)) instrumentInfo(object@connection)
		}
)
setMethod(
		'ionisation', 'msgfRaw',
		function(object){
			if(isLoaded(object)) ionisation(object@connection)
		}
)
setMethod(
		'manufacturer', 'msgfRaw',
		function(object){
			if(isLoaded(object)) manufacturer(object@connection)
		}
)
setMethod(
		'model', 'msgfRaw',
		function(object){
			if(isLoaded(object)) model(object@connection)
		}
)
setMethod(
		'peaksCount', 'msgfRaw',
		function(object, ...){
			if(isLoaded(object)) peaksCount(object@connection, ...)
		}
)
setMethod(
		'peaks', 'msgfRaw',
		function(object, ...){
			if(isLoaded(object)) peaks(object@connection, ...)
		}
)
setMethod(
		'runInfo', 'msgfRaw',
		function(object){
			if(isLoaded(object)) runInfo(object@connection)
		}
)
msgfRaw <- function(file, connect=FALSE){
	if(missing(file)){
		new(Class='msgfRaw')
	} else {
		if(connect){
			new(Class='msgfRaw', location=file, connection=openMSfile(file))
		} else {
			new(Class='msgfRaw', location=file)
		}
	}
}
msgfRawFromID <- function(file, connect=FALSE){
	parsedMZID <- xmlTreeParse(file, useInternalNodes=TRUE)
	rootMZID <- xmlRoot(parsedMZID)
	rawfile <- xmlAttrs(rootMZID[['DataCollection']][['Inputs']][['SpectraData']])['location']
	if(!file.exists(rawfile)){
		rawfile <- file.path(dirname(file), basename(rawfile))
		if(!file.exists(rawfile)){
			rawfile <- ''
		} else {}
	} else {}
	msgfRaw(file=rawfile, connect=connect)
}