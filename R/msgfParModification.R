#' A class handling modification characteristics
#' 
#' This class defines a single modification type that can be searched for by 
#' MS-GF+. Modifications are collected in a 
#' \code{\link{msgfParModificationList}} before adding them to 
#' \code{\linkS4class{msgfPar}} objects.
#' 
#' @slot composition The molecular formula for the modification.
#' @slot mass The monoisotopic mass of the modification
#' @slot residues The amino acids the modification applies to
#' @slot type Whether the modification is optional or always present
#' @slot position The possibel position of the modification
#' @slot name The name of the modification
#' 
#' @examples
#' # Using composition
#' modification1 <- msgfParModification(
#'                                      name='Carbamidomethyl',
#'                                      composition='C2H3N1O1',
#'                                      residues='C',
#'                                      type='fix',
#'                                      position='any'
#'                                     )
#' # Using exact mass
#' modification2 <- msgfParModification(
#'                                      name='Oxidation',
#'                                      mass=15.994915,
#'                                      residues='M',
#'                                      type='opt',
#'                                      position='any'
#'                                     )
#' 
#' @family msgfParClasses
#' 
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
					return('Either mass or composition must be specified')
				} else {}
			} else {
				compSplit <- gregexpr('[A-Z][a-z]?', object@composition, perl=TRUE)
				if(!all(regmatches(object@composition, compSplit)[[1]] %in% c('C', 'H', 'N', 'O', 'S', 'P', 'Br', 'Cl', 'Fe', ''))){
					return('Modification must consist of only \'C\', \'H\', \'N\', \'O\', \'S\', \'P\', \'Br\', \'Cl\' and \'Fe\'')
				} else {}
			}
			if(object@residues != '*'){
				res <- strsplit(object@residues, '')[[1]]
				if(!all(toupper(res) %in% c('A', 'R', 'N', 'D', 'C', 'E', 'Q', 'G', 'H', 'I', 'L', 'K', 'M', 'F', 'P', 'S', 'T', 'W', 'Y', 'V'))){
					return('Residues must be one of the 20 common amino acids in one letter code')
				} else {}
			} else {}
			if(!(tolower(object@type) %in% c('fix', 'opt'))){
				return('The type of the modification must be either \'fix\' for fixed or \'opt\' for optional')
			} else {}
			if(!(tolower(object@position) %in% c('any', 'n-term', 'nterm', 'c-term', 'cterm', 'prot-n-term', 'protnterm', 'prot-c-term', 'protcterm'))){
				return('Position must be either \'any\', \'n-term\', \'c-term\', \'prot-n-term\' or \'prot-c-term\'')
			} else {}
		}
)
#' @describeIn msgfParModification Get \code{\link[base]{system}} compliant 
#' function call
#' 
#' @return For getMSGFpar() A string.
#' 
setMethod(
		'getMSGFpar', 'msgfParModification',
		function(object){
			if(object@composition != ''){
				compSplit <- gregexpr('[- | \\d]+', object@composition, perl=TRUE)
				compOrder <- order(match(regmatches(object@composition, compSplit, invert=TRUE)[[1]], c('C', 'H', 'N', 'O', 'S', 'P', 'Br', 'Cl', 'Fe', '')))
				compOrdered <- paste(regmatches(object@composition, compSplit, invert=TRUE)[[1]][compOrder], regmatches(object@composition, compSplit)[[1]][compOrder], sep='')[1:length(compSplit[[1]])]
				ans <- paste(compOrdered, collapse='')
			} else {
				ans <- paste(object@mass)
			}
			ans <- paste(ans, toupper(object@residues), object@type, object@position, object@name, sep=',')
			ans
		}
)
#' @describeIn msgfParModification Short summary of msgfParModification object
#' 
#' @param object An msgfParModification object
#' 
setMethod(
		'show', 'msgfParModification',
		function(object){
			cat(object@name, ':\t', if(object@composition != '') object@composition else object@mass, ', ', object@residues, ', ', object@type, ', ', object@position, '\n', sep='')
		}
)
#' @rdname msgfParModification-class
#' 
#' @param name The name of the modification
#' 
#' @param composition The molecular formular as a string for the modification. 
#' Loss of atoms are denoted with negative integers (e.g. O-1 for loss of 
#' oxygen)
#' 
#' @param mass The monoisotopic mass change between a peptide without and with
#' the given modification. Either composition or mass must be defined.
#' 
#' @param residues The amino acids that the modification applies to. Given as 
#' their one-letter code in upper-case without any separation. Use '*' for all
#' residues
#' 
#' @param type Either 'fix' or 'opt' for fixed or optional
#' 
#' @param position Where the modification can be. Either 'any', 'c-term',
#' 'n-term', 'prot-c-term' or 'prot-n-term.
#' 
#' @return For msgfParModification() An msgfParModification object.
#' 
#' @export
#' 
msgfParModification <- function(name, composition='', mass=as.numeric(NA), residues, type, position){
  if(type == 'fixed') type <- 'fix'
  if(type == 'optional') type <- 'opt'
	new(Class='msgfParModification', name=name, composition=composition, mass=mass, residues=residues, type=type, position=position)
}
