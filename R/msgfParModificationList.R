#' @include msgfParModification.R
NULL

setClass(
		'msgfParModificationList',
		representation=representation(
				nMod='numeric',
				modifications='list'
		),
		validity=function(object){
			if(length(object@nMod) != 1){
				stop('nMod must be of length 1')
			} else if(object@nMod != floor(object@nMod)){
				stop('nMod must be an integer')
			} else if(length(object@modifications) != 0){
				if(!all(sapply(object@modifications, class) == 'msgfParModification')){
					stop('Modifications should be supplied as a list of modification objects')
				} else {}
			} else {
				return(TRUE)
			}
		},
		prototype=prototype(
				nMod=as.numeric(NA),
				modifications=list()
		)
)
setMethod(
		'show', 'msgfParModificationList',
		function(object){
			if(length(object) == 0){
				cat('An empty msgfParModificationList object\n')
			} else {
				cat('Number of modifications per peptide: ', object@nMod, '\n\n', sep='')
				for(i in 1:length(object@modifications)){
					show(object@modifications[[i]])
				}				
			}
		}
)
setMethod(
		'length', 'msgfParModificationList',
		function(x){
			length(x@modifications)
		}
)
setMethod(
		'getMSGFpar', 'msgfParModificationList',
		function(object){
			if(length(object) != 0){
				modFile <- file.path(system.file(package='MSGFplus'), 'modification_temp.txt')
				unlink(modFile, force=TRUE)
				sink(modFile)
				cat('NumMods=', if(is.na(object@nMod)) 2 else object@nMod, sep='')
				cat('\n\n')
				for(i in 1:length(object)){
					par <- getMSGFpar(object@modifications[[i]])
					cat(par)
					cat('\n')
				}
				sink()
				paste('-mod ', modFile)
			} else {
				''
			}			
		}
)
setMethod(
    '[[', c('msgfParModificationList', 'numeric', 'missing'),
    function(x, i, j, ...) {
        x@modifications[[i]]
    }
)
setReplaceMethod(
    '[[', c('msgfParModificationList', 'numeric', 'missing', 'msgfParModification'),
    function(x, i, j, ..., value) {
        x@modifications[[i]] <- value
        x
    }
)
msgfParModificationList <- function(nMod, modifications=list()){
	if(length(modifications) == 0){
		new(Class='msgfParModificationList')
	} else {
		modifications <- lapply(modifications, function(x){
					if(class(x) != 'msgfParModification'){
						do.call('msgfParModification', as.list(x))
					} else {
						x
					}
				})
		new(Class='msgfParModificationList', nMod=nMod, modifications=modifications)		
	}
}
