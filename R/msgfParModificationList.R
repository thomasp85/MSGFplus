#' @include msgfParModification.R
NULL

#' A class handling a list of modifications
#' 
#' This class defines a set of modifications and a maximum number of 
#' modifications allowed per peptide.
#' 
#' @slot nMod The maximum allowed number of modifications to expect on any 
#' peptide
#' @slot modifications A list of \code{\linkS4class{msgfParModification}} 
#' objects
#' 
#' @examples
#' modification1 <- msgfParModification(
#'                                      name='Carbamidomethyl',
#'                                      composition='C2H3N1O1',
#'                                      residues='C',
#'                                      type='fix',
#'                                      position='any'
#'                                     )
#' modification2 <- msgfParModification(
#'                                      name='Oxidation',
#'                                      mass=15.994915,
#'                                      residues='M',
#'                                      type='opt',
#'                                      position='any'
#'                                     )
#' modificationlist <- msgfParModificationList(
#'                                             nMod=2,
#'                                             modifications=list(
#'                                                 modification1,
#'                                                 modification2
#'                                             )
#'                                            )
#' modificationlist[[3]] <- msgfParModification(
#'                                              name='Gln->pyro-Glu',
#'                                              composition='H-3N-1',
#'                                              residues='Q',
#'                                              type='opt',
#'                                              position='N-term'
#'                                             )
#' @family msgfParClasses
#' 
setClass(
		'msgfParModificationList',
		representation=representation(
				nMod='numeric',
				modifications='list'
		),
		validity=function(object){
			if(length(object@nMod) != 1){
				return('nMod must be of length 1')
			} else if(object@nMod != floor(object@nMod)){
				return('nMod must be an integer')
			} else if(length(object@modifications) != 0){
				if(!all(sapply(object@modifications, class) == 'msgfParModification')){
					return('Modifications should be supplied as a list of modification objects')
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
#' @describeIn msgfParModificationList Short summary of msgfParModificationList 
#' object
#' 
#' @param object An msgfParModificationList object
#' 
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
#' @describeIn msgfParModificationList Report the length of an 
#' msgfParModificationList object
#' 
#' @param x An msgfParModificationList object
#' 
#' @return For length() An integer.
#' 
setMethod(
		'length', 'msgfParModificationList',
		function(x){
			length(x@modifications)
		}
)
#' @describeIn msgfParModificationList Get \code{\link[base]{system}} compliant 
#' function call
#' 
#' @return For getMSGFpar() A string.
#' 
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
				if(Sys.info()["sysname"] == 'Windows'){
				    modFile <- paste0('\"', modFile, '\"')
				}
				paste('-mod', modFile)
			} else {
				''
			}			
		}
)
#' @describeIn msgfParModificationList Get the i'th modification
#' 
#' @param i The index of the modification
#' 
#' @param j Ignored
#' 
#' @param ... Ignored
#' 
#' @return For '[[' A msgfParModification object
#' 
setMethod(
    '[[', c('msgfParModificationList', 'numeric', 'missing'),
    function(x, i, j, ...) {
        x@modifications[[i]]
    }
)
#' @describeIn msgfParModificationList Set or change the i'th modification
#' 
#' @param value An msgfParModification object
#' 
setMethod(
    '[[<-', c('msgfParModificationList', 'numeric', 'missing', 'msgfParModification'),
    function(x, i, j, ..., value) {
        x@modifications[[i]] <- value
        x
    }
)
#' @rdname msgfParModificationList-class
#' 
#' @param nMod The maximum allowed number of modifications to expect on any 
#' peptide
#' 
#' @param modifications A list of \code{\linkS4class{msgfParModification}} 
#' objects
#' 
#' @return For msgfParModificationList() An msgfParModificationList object.
#' 
#' @export
#' 
msgfParModificationList <- function(nMod, modifications=list()){
	if(length(modifications) == 0){
		mods <- new(Class='msgfParModificationList')
        if(!missing(nMod)) mods@nMod <- nMod
        mods
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
