# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setClass(
		Class='msgfRes',
		representation=representation(
				location = 'character',
				data = 'data.frame'
		),
		validity=function(object){
			if(object@location != ''){
				if(!file.exists(object@location)){
					stop('File does not exist')
				} else if(!checkResFile(object)){
					stop('Only output from MS-GF+ supported')
				}
			}
		},
		prototype=prototype(
			location='',
			data=data.frame()
		)
)

setMethod(
	'show', 'msgfRes',
	function(object){
		show(object@data)
	}
)
setMethod(
	'length', 'msgfRes',
	function(x){
		length(x@data)
	}
)
setAs(
	'msgfRes', 'data.frame',
	function(from){
		from@data
	}
)
as.data.frame.msgfRes <- function(x) as(x, 'data.frame')
setMethod(
	'[', 'msgfRes',
	function(x, i, j){
		`[`(as.data.frame(x), i, j)
	}
)
checkResFile <- function(file){
	parsedMZID <- xmlTreeParse(file, useInternalNodes=TRUE)
	rootMZID <- xmlRoot(parsedMZID)
	software <- xmlAttrs(rootMZID[['AnalysisSoftwareList']][['AnalysisSoftware']])['name']
	free(parsedMZID)
	as.vector(software) == 'MS-GF+'
}
analysisSoftwareVersion <- function(file){
	parsedMZID <- xmlTreeParse(file, useInternalNodes=TRUE)
	rootMZID <- xmlRoot(parsedMZID)
	version <- xmlAttrs(rootMZID[['AnalysisSoftwareList']][['AnalysisSoftware']])['version']
	free(parsedMZID)
	as.vector(version)
}
setMethod(
	'checkResFile', 'msgfRes',
	function(object){
		checkResFile(object@location)
	}
)
setMethod(
		'analysisSoftwareVersion', 'msgfRes',
		function(object){
			analysisSoftwareVersion(object@location)
		}
)
msgfRes <- function(file, memory=10000, showDecoy=TRUE, showQValue=TRUE, unroll=TRUE){
	if(!checkResFile(file)){
		stop('Only output from MS-GF+ supported')
	} else {}
	if(Sys.info()["sysname"] == 'Windows'){
		file1 <- paste('\"', file, '\"', sep='')
	} else {
		file1 <- gsub(' ', '\\ ', file, fixed=T)
	}
	unlink(R.home(component='library/MSGFplus/msgfTSVConvertTemp.tsv'), force=TRUE)
	callConv <- paste('java -Xmx', memory, 'M -cp ', R.home(component='library/MSGFplus/MSGFplus/MSGFplus.jar'), ' edu.ucsd.msjava.ui.MzIDToTsv -i ', file1, ' -o ', R.home(component='library/MSGFplus/msgfTSVConvertTemp.tsv'), sep='')
	if(showDecoy){
		callConv <- paste(callConv, ' -showDecoy 1', sep='')
	} else {
		callConv <- paste(callConv, ' -showDecoy 0', sep='')
	}
	if(showQValue){
		callConv <- paste(callConv, ' -showQValue 1', sep='')
	} else {
		callConv <- paste(callConv, ' -showQValue 0', sep='')
	}
	if(unroll){
		callConv <- paste(callConv, ' -unroll 1', sep='')
	} else {
		callConv <- paste(callConv, ' -unroll 0', sep='')
	}
	error <- system(callConv)
	
	if(error==0){
		if(length(scan(R.home(component='library/MSGFplus/msgfTSVConvertTemp.tsv'), skip=1, nlines=1, what='character', quiet=T)) == 0){
			warning(paste('No peptides detected in ', basename(file), sep=''))
			ans <- data.frame()
		} else {
			ans <- read.table(R.home(component='library/MSGFplus/msgfTSVConvertTemp.tsv'), sep='\t', header=TRUE, comment.char='')
			names(ans) <- sub('#', '', scan(R.home(component='library/MSGFplus/msgfTSVConvertTemp.tsv'), nlines=1, what=character(), quiet=TRUE))
			ans$ScanNum <- ifelse(ans$ScanNum == -1, as.numeric(sub('.*scanId=([\\d]+).*', '\\1', ans$SpecID, perl=TRUE)), ans$ScanNum)
		}
		unlink(R.home(component='library/MSGFplus/msgfTSVConvertTemp.tsv'))
		
		new(Class='msgfRes', location=file, data=ans)		
	} else {
		warning('An error occurred during file reading')
		new(Class='msgfRes')
	}
}