#' @include aaa.R
#' @include generics.R
#' @include msgfPar.R
#' @include msgfParChargeRange.R
#' @include msgfParEnzyme.R
#' @include msgfParFragmentation.R
#' @include msgfParInstrument.R
#' @include msgfParIsotopeError.R
#' @include msgfParLengthRange.R
#' @include msgfParMatches.R
#' @include msgfParModificationList.R
#' @include msgfParNtt.R
#' @include msgfParProtocol.R
#' @include msgfParTda.R
#' @include msgfParTolerance.R
NULL

#' @describeIn db Get the database location
#' 
setMethod(
    'db', 'msgfPar',
    function(object){
        object@database
    }
)
#' @describeIn db Set the database location
#' 
setMethod(
    'db<-', c('msgfPar', 'character'),
    function(object, value){
        object@database <- value
        validObject(object)
        object
    }
)
#' @describeIn chargeRange Get the charge range
#' 
setMethod(
    'chargeRange', 'msgfPar',
    function(object){
        res <- object@chargeRange@value
        names(res) <- c('min', 'max')
        res
    }
)
#' @describeIn chargeRange Set the charge range using lower and upper bounds
#' 
setMethod(
    'chargeRange<-', c('msgfPar', 'numeric'),
    function(object, value) {
        object@chargeRange <- msgfParChargeRange(value)
        object
    }
)
#' @describeIn chargeRange Set the charge range using a dedicated
#' msgfParChargeRange object
#' 
setMethod(
    'chargeRange<-', c('msgfPar', 'msgfParChargeRange'),
    function(object, value) {
        object@chargeRange <- value
        object
    }
)
#' @describeIn enzyme Get the enzyme currently used
#' 
setMethod(
    'enzyme', 'msgfPar',
    function(object){
        res <- object@enzyme@enzyme
        names(res) <- enzymeLookup()$Description[enzymeLookup()$Index==res]
        res
    }
)
#' @describeIn enzyme Set the enzyme to use using the key for the enzyme
#' 
setMethod(
    'enzyme<-', c('msgfPar', 'numeric'),
    function(object, value) {
        object@enzyme <- msgfParEnzyme(value)
        object
    }
)
#' @describeIn enzyme Set the enzyme to use using the name of the enzyme
#' 
setMethod(
    'enzyme<-', c('msgfPar', 'character'),
    function(object, value) {
        object@enzyme <- msgfParEnzyme(value)
        object
    }
)
#' @describeIn enzyme Set the enzyme to use using an msgfParEnzyme object
#' 
setMethod(
    'enzyme<-', c('msgfPar', 'msgfParEnzyme'),
    function(object, value) {
        object@enzyme <- value
        object
    }
)
#' @describeIn fragmentation Get the fragmentation method currently used
#' 
setMethod(
    'fragmentation', 'msgfPar',
    function(object){
        res <- object@fragmentation@method
        names(res) <- fragmentationLookup()$Description[fragmentationLookup()$Index==res]
        res
    }
)
#' @describeIn fragmentation Set the fragmentation method using the key for the 
#' method
#' 
setMethod(
    'fragmentation<-', c('msgfPar', 'numeric'),
    function(object, value) {
        object@fragmentation <- msgfParFragmentation(value)
        object
    }
)
#' @describeIn fragmentation Set the fragmentation method using the name of the 
#' method
#' 
setMethod(
    'fragmentation<-', c('msgfPar', 'character'),
    function(object, value) {
        object@fragmentation <- msgfParFragmentation(value)
        object
    }
)
#' @describeIn fragmentation Set the fragmentation method using an 
#' msgfParFragmentation object
#' 
setMethod(
    'fragmentation<-', c('msgfPar', 'msgfParFragmentation'),
    function(object, value) {
        object@fragmentation <- value
        object
    }
)
#' @describeIn instrument Get the instrument currently used
#' 
setMethod(
    'instrument', 'msgfPar',
    function(object){
        res <- object@instrument@instrument
        names(res) <- instrumentLookup()$Description[instrumentLookup()$Index==res]
        res
    }
)
#' @describeIn instrument Set the instrument using the key for the instrument
#' 
setMethod(
    'instrument<-', c('msgfPar', 'numeric'),
    function(object, value) {
        object@instrument <- msgfParInstrument(value)
        object
    }
)
#' @describeIn instrument Set the instrument using the name of the instrument
#' 
setMethod(
    'instrument<-', c('msgfPar', 'character'),
    function(object, value) {
        object@instrument <- msgfParInstrument(value)
        object
    }
)
#' @describeIn instrument Set the instrument using an msgfParInstrument object
#' 
setMethod(
    'instrument<-', c('msgfPar', 'msgfParInstrument'),
    function(object, value) {
        object@instrument <- value
        object
    }
)
#' @describeIn isotopeError Get the isotope error currently used
#' 
setMethod(
    'isotopeError', 'msgfPar',
    function(object){
        object@isotopeError@range
    }
)
#' @describeIn isotopeError Set the isotope error with an integer vector
#' 
setMethod(
    'isotopeError<-', c('msgfPar', 'numeric'),
    function(object, value) {
        object@isotopeError <- msgfParIsotopeError(value)
        object
    }
)
#' @describeIn isotopeError Set the isotope error with an msgfParIsotopeError 
#' object
#' 
setMethod(
    'isotopeError<-', c('msgfPar', 'msgfParIsotopeError'),
    function(object, value) {
        object@isotopeError <- value
        object
    }
)
#' @describeIn lengthRange Get the lower and upper bounds of peptide lengths
#' 
setMethod(
    'lengthRange', 'msgfPar',
    function(object){
        res <- object@lengthRange@value
        names(res) <- c('min', 'max')
        res
    }
)
#' @describeIn lengthRange Set the lower and upper bounds of peptide lengths 
#' using an integer vector
#' 
setMethod(
    'lengthRange<-', c('msgfPar', 'numeric'),
    function(object, value) {
        object@lengthRange <- msgfParLengthRange(value)
        object
    }
)
#' @describeIn lengthRange Set the lower and upper bounds of peptide lengths 
#' using an msgfParLengthRange
#' 
setMethod(
    'lengthRange<-', c('msgfPar', 'msgfParLengthRange'),
    function(object, value) {
        object@lengthRange <- value
        object
    }
)
#' @describeIn matches Get the number of matches reported per spectrum
#' 
setMethod(
    'matches', 'msgfPar',
    function(object){
        res <- object@matches@value
        res
    }
)
#' @describeIn matches Set the number of matches reported per spectrum using an
#' integer
#' 
setMethod(
    'matches<-', c('msgfPar', 'numeric'),
    function(object, value) {
        object@matches <- msgfParMatches(value)
        object
    }
)
#' @describeIn matches Set the number of matches reported per spectrum using an
#' msgfParMatches object
#' 
setMethod(
    'matches<-', c('msgfPar', 'msgfParMatches'),
    function(object, value) {
        object@matches <- value
        object
    }
)
#' @describeIn ntt Get the number of tolerable termini
#' 
setMethod(
    'ntt', 'msgfPar',
    function(object){
        res <- object@ntt@value
        res
    }
)
#' @describeIn ntt Set the ntt using an integer
#' 
setMethod(
    'ntt<-', c('msgfPar', 'numeric'),
    function(object, value) {
        object@ntt <- msgfParNtt(value)
        object
    }
)
#' @describeIn ntt Set the ntt using an msgfParNtt object
#' 
setMethod(
    'ntt<-', c('msgfPar', 'msgfParNtt'),
    function(object, value) {
        object@ntt <- value
        object
    }
)
#' @describeIn protocol Get the protocol currently used
#' 
setMethod(
    'protocol', 'msgfPar',
    function(object){
        res <- object@protocol@protocol
        names(res) <- protocolLookup()$Description[protocolLookup()$Index==res]
        res
    }
)
#' @describeIn protocol Set the protocol using the key for the protocol
#' 
setMethod(
    'protocol<-', c('msgfPar', 'numeric'),
    function(object, value) {
        object@protocol <- msgfParProtocol(value)
        object
    }
)
#' @describeIn protocol Set the protocol using the name of the protocol
#' 
setMethod(
    'protocol<-', c('msgfPar', 'character'),
    function(object, value) {
        object@protocol <- msgfParProtocol(value)
        object
    }
)
#' @describeIn protocol Set the protocol using an msgfParProtocol object
#' 
setMethod(
    'protocol<-', c('msgfPar', 'msgfParProtocol'),
    function(object, value) {
        object@protocol <- value
        object
    }
)
#' @describeIn tda Get whether tda is currently used for FDR estimation
#' 
setMethod(
    'tda', 'msgfPar',
    function(object){
        res <- object@tda@tda
        res
    }
)
#' @describeIn tda Set the use of tda using a boolean (TRUE/FALSE)
#' 
setMethod(
    'tda<-', c('msgfPar', 'logical'),
    function(object, value) {
        object@tda <- msgfParTda(value)
        object
    }
)
#' @describeIn tda Set the use of tda using an msgfParTda object
#' 
setMethod(
    'tda<-', c('msgfPar', 'msgfParTda'),
    function(object, value) {
        object@tda <- value
        object
    }
)
#' @describeIn tolerance Get the lower and upper bounds of the tolerance
#' 
setMethod(
    'tolerance', 'msgfPar',
    function(object){
        res <- c(low=object@tolerance@low, high=object@tolerance@high)
        paste(res, object@tolerance@unit)
    }
)
#' @describeIn tolerance Get the lower and upper bounds of the tolerance
#' 
setMethod(
    'toleranceRange', 'msgfPar',
    function(object){
        res <- c(low=object@tolerance@low, high=object@tolerance@high)
        res
    }
)
#' @describeIn tolerance Get the unit the tolerance is measured in
#' 
setMethod(
    'toleranceUnit', 'msgfPar',
    function(object){
        res <- object@tolerance@unit
        res
    }
)
#' @describeIn tolerance Set the lower and upper bounds of the tolerance using a
#' numeric vector of length 2
#' 
setMethod(
    'toleranceRange<-', c('msgfPar', 'numeric'),
    function(object, value) {
        if(length(value) == 1) {
            object@tolerance <- msgfParTolerance(value, unit=toleranceUnit(object))
        } else {
            object@tolerance <- msgfParTolerance(low=value[1], high=value[2], unit=toleranceUnit(object))
        }
        object
    }
)
#' @describeIn tolerance Set the unit the tolerance is meassured in
#' 
setMethod(
    'toleranceUnit<-', c('msgfPar', 'character'),
    function(object, value) {
        range = toleranceRange(object)
        object@tolerance <- msgfParTolerance(low=range[1], high=range[2], unit=value)
        object
    }
)
#' @describeIn tolerance Set the lower and upper bounds of the tolerance using a
#' character vector of length 2, where each element is of the form '<value> 
#' <unit>'
#' 
setMethod(
    'tolerance<-', c('msgfPar', 'character'),
    function(object, value) {
        value <- strsplit(value, ' ')
        if(length(value) == 1) {
            object@tolerance <- msgfParTolerance(as.numeric(value[[1]][1]), unit=value[[1]][2])
        } else {
            object@tolerance <- msgfParTolerance(low=as.numeric(value[[1]][1]), high=as.numeric(value[[2]][1]), unit=value[[1]][2])
        }
        object
    }
)
#' @describeIn tolerance Set the lower and upper bounds of the tolerance using 
#' an msgfParTolerance object
#' 
setMethod(
    'tolerance<-', c('msgfPar', 'msgfParTolerance'),
    function(object, value) {
        object@tolerance <- value
        object
    }
)
#' @describeIn mods Get the list of modifications allowed during 
#' peptide search
#' 
setMethod(
    'mods', 'msgfPar',
    function(object){
        object@modification
    }
)
#' @describeIn mods Set the list of modifications allowed during 
#' peptide search
#' 
setMethod(
    'mods<-', c('msgfPar', 'msgfParModificationList'),
    function(object, value) {
        object@modification <- value
        object
    }
)
#' @describeIn mods Get the number of peptides allowed per peptide 
#' during search
#' 
setMethod(
    'nMod', 'msgfPar',
    function(object){
        object@modification@nMod
    }
)
#' @describeIn mods Set the number of peptides allowed per peptide 
#' during search using an integer
#' 
setMethod(
    'nMod<-', c('msgfPar', 'numeric'),
    function(object, value) {
        object@modification@nMod <- value
        object
    }
)