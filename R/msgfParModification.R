setClass(
		'msgfParModification',
		representation=representation(
				composition='character',
				mass='numeric',
				residues='character',
				type='character',
				position='character',
				name='character'
		),
		validity=function(object){
			if(object@composition==''){
				if(is.na(object@mass)){
					stop('Either mass or composition must be specified')
				} else {}
			} else {
				compSplit <- gregexpr('[A-Z][a-z]?', object@composition, perl=T)
				if(!all(regmatches(object@composition, compSplit)[[1]] %in% c('C', 'H', 'N', 'O', 'S', 'P', 'Br', 'Cl', 'Fe', ''))){
					stop('Modification must consist of only \'C\', \'H\', \'N\', \'O\', \'S\', \'P\', \'Br\', \'Cl\' and \'Fe\'')
				} else {}
			}
			if(object@residues != '*'){
				res <- strsplit(object@residues, '')[[1]]
				if(!all(toupper(res) %in% c('A', 'R', 'N', 'D', 'C', 'E', 'Q', 'G', 'H', 'I', 'L', 'K', 'M', 'F', 'P', 'S', 'T', 'W', 'Y', 'V'))){
					stop('Residues must be one of the 20 common amino acids in one letter code')
				} else {}
			} else {}
			if(!(tolower(object@type) %in% c('fix', 'opt'))){
				stop('The type of the modification must be either \'fix\' for fixed or \'opt\' for optional')
			} else {}
			if(!(tolower(object@position) %in% c('any', 'n-term', 'nterm', 'c-term', 'cterm', 'prot-n-term', 'protnterm', 'prot-c-term', 'protcterm'))){
				stop('Position must be either \'any\', \'n-term\', \'c-term\', \'prot-n-term\' or \'prot-c-term\'')
			} else {}
		}
)
setMethod(
		'getMSGFpar', 'msgfParModification',
		function(object){
			if(object@composition != ''){
				compSplit <- gregexpr('[- | \\d]+', object@composition, perl=T)
				compOrder <- order(match(regmatches(object@composition, compSplit, invert=T)[[1]], c('C', 'H', 'N', 'O', 'S', 'P', 'Br', 'Cl', 'Fe', '')))
				compOrdered <- paste(regmatches(object@composition, compSplit, invert=T)[[1]][compOrder], regmatches(object@composition, compSplit)[[1]][compOrder], sep='')[1:length(compSplit[[1]])]
				ans <- paste(compOrdered, collapse='')
			} else {
				ans <- paste(object@mass)
			}
			ans <- paste(ans, toupper(object@residues), object@type, object@position, object@name, sep=',')
			ans
		}
)
setMethod(
		'show', 'msgfParModification',
		function(object){
			cat(object@name, ':\t', if(object@composition != '') object@composition else object@mass, ', ', object@residues, ', ', object@type, ', ', object@position, '\n', sep='')
		}
)

#' @export
#' 
msgfParModification <- function(name, composition='', mass=as.numeric(NA), residues, type, position){
  if(type == 'fixed') type <- 'fix'
  if(type == 'optional') type <- 'opt'
	new(Class='msgfParModification', name=name, composition=composition, mass=mass, residues=residues, type=type, position=position)
}
