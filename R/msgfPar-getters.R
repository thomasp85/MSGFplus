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

#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'database', 'msgfPar',
    function(object){
        object@database
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name database<-
#' 
setReplaceMethod(
    'database', 'msgfPar',
    function(object, value){
        object@database <- value
        validObject(object)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'chargeRange', 'msgfPar',
    function(object){
        res <- object@chargeRange@value
        names(res) <- c('min', 'max')
        res
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name chargeRange<-
#' 
setReplaceMethod(
    'chargeRange', c('msgfPar', 'numeric'),
    function(object, value) {
        object@chargeRange <- msgfParChargeRange(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name chargeRange<-
#' 
setReplaceMethod(
    'chargeRange', c('msgfPar', 'msgfParChargeRange'),
    function(object, value) {
        object@chargeRange <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'enzyme', 'msgfPar',
    function(object){
        res <- object@enzyme@enzyme
        names(res) <- enzymeLookup()$Description[enzymeLookup()$Index==res]
        res
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name enzyme<-
#' 
setReplaceMethod(
    'enzyme', c('msgfPar', 'numeric'),
    function(object, value) {
        object@enzyme <- msgfParEnzyme(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name enzyme<-
#' 
setReplaceMethod(
    'enzyme', c('msgfPar', 'character'),
    function(object, value) {
        object@enzyme <- msgfParEnzyme(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name enzyme<-
#' 
setReplaceMethod(
    'enzyme', c('msgfPar', 'msgfParEnzyme'),
    function(object, value) {
        object@enzyme <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'fragmentation', 'msgfPar',
    function(object){
        res <- object@fragmentation@method
        names(res) <- fragmentationLookup()$Description[fragmentationLookup()$Index==res]
        res
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name fragmentation<-
#' 
setReplaceMethod(
    'fragmentation', c('msgfPar', 'numeric'),
    function(object, value) {
        object@fragmentation <- msgfParFragmentation(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name fragmentation<-
#' 
setReplaceMethod(
    'fragmentation', c('msgfPar', 'character'),
    function(object, value) {
        object@fragmentation <- msgfParFragmentation(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name fragmentation<-
#' 
setReplaceMethod(
    'fragmentation', c('msgfPar', 'msgfParFragmentation'),
    function(object, value) {
        object@fragmentation <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'instrument', 'msgfPar',
    function(object){
        res <- object@instrument@instrument
        names(res) <- instrumentLookup()$Description[instrumentLookup()$Index==res]
        res
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name instrument<-
#' 
setReplaceMethod(
    'instrument', c('msgfPar', 'numeric'),
    function(object, value) {
        object@instrument <- msgfParInstrument(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name instrument<-
#' 
setReplaceMethod(
    'instrument', c('msgfPar', 'character'),
    function(object, value) {
        object@instrument <- msgfParInstrument(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name instrument<-
#' 
setReplaceMethod(
    'instrument', c('msgfPar', 'msgfParInstrument'),
    function(object, value) {
        object@instrument <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'isotopeError', 'msgfPar',
    function(object){
        object@isotopeError@range
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name isotopeError<-
#' 
setReplaceMethod(
    'isotopeError', c('msgfPar', 'numeric'),
    function(object, value) {
        object@isotopeError <- msgfParIsotopeError(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name isotopeError<-
#' 
setReplaceMethod(
    'isotopeError', c('msgfPar', 'msgfParIsotopeError'),
    function(object, value) {
        object@isotopeError <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'lengthRange', 'msgfPar',
    function(object){
        res <- object@lengthRange@value
        names(res) <- c('min', 'max')
        res
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name lengthRange<-
#' 
setReplaceMethod(
    'lengthRange', c('msgfPar', 'numeric'),
    function(object, value) {
        object@lengthRange <- msgfParLengthRange(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name lengthRange<-
#' 
setReplaceMethod(
    'lengthRange', c('msgfPar', 'msgfParLengthRange'),
    function(object, value) {
        object@lengthRange <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'matches', 'msgfPar',
    function(object){
        res <- object@matches@value
        res
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name matches<-
#' 
setReplaceMethod(
    'matches', c('msgfPar', 'numeric'),
    function(object, value) {
        object@matches <- msgfParMatches(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name matches<-
#' 
setReplaceMethod(
    'matches', c('msgfPar', 'msgfParMatches'),
    function(object, value) {
        object@matches <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'ntt', 'msgfPar',
    function(object){
        res <- object@ntt@value
        res
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name ntt<-
#' 
setReplaceMethod(
    'ntt', c('msgfPar', 'numeric'),
    function(object, value) {
        object@ntt <- msgfParNtt(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name ntt<-
#' 
setReplaceMethod(
    'ntt', c('msgfPar', 'msgfParNtt'),
    function(object, value) {
        object@ntt <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'protocol', 'msgfPar',
    function(object){
        res <- object@protocol@protocol
        names(res) <- protocolLookup()$Description[protocolLookup()$Index==res]
        res
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name protocol<-
#' 
setReplaceMethod(
    'protocol', c('msgfPar', 'numeric'),
    function(object, value) {
        object@protocol <- msgfParProtocol(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name protocol<-
#' 
setReplaceMethod(
    'protocol', c('msgfPar', 'character'),
    function(object, value) {
        object@protocol <- msgfParProtocol(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name protocol<-
#' 
setReplaceMethod(
    'protocol', c('msgfPar', 'msgfParProtocol'),
    function(object, value) {
        object@protocol <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'tda', 'msgfPar',
    function(object){
        res <- object@tda@tda
        res
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name tda<-
#' 
setReplaceMethod(
    'tda', c('msgfPar', 'logical'),
    function(object, value) {
        object@tda <- msgfParTda(value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name tda<-
#' 
setReplaceMethod(
    'tda', c('msgfPar', 'msgfParTda'),
    function(object, value) {
        object@tda <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'tolerance', 'msgfPar',
    function(object){
        res <- c(low=object@tolerance@low, high=object@tolerance@high)
        paste(res, object@tolerance@unit)
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'toleranceRange', 'msgfPar',
    function(object){
        res <- c(low=object@tolerance@low, high=object@tolerance@high)
        res
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'toleranceUnit', 'msgfPar',
    function(object){
        res <- object@tolerance@unit
        res
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name toleranceRange<-
#' 
setReplaceMethod(
    'toleranceRange', c('msgfPar', 'numeric'),
    function(object, value) {
        if(length(value) == 1) {
            object@tolerance <- msgfParTolerance(value, unit=toleranceUnit(object))
        } else {
            object@tolerance <- msgfParTolerance(low=value[1], high=value[2], unit=toleranceUnit(object))
        }
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name toleranceUnit<-
#' 
setReplaceMethod(
    'toleranceUnit', c('msgfPar', 'character'),
    function(object, value) {
        range = toleranceRange(object)
        object@tolerance <- msgfParTolerance(low=range[1], high=range[2], unit=value)
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name tolerance<-
#' 
setReplaceMethod(
    'tolerance', c('msgfPar', 'character'),
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
#' @rdname msgfPar_get_set-methods
#' 
#' @name tolerance<-
#' 
setReplaceMethod(
    'tolerance', c('msgfPar', 'msgfParTolerance'),
    function(object, value) {
        object@tolerance <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'modifications', 'msgfPar',
    function(object){
        object@modification
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name modifications<-
#' 
setReplaceMethod(
    'modifications', c('msgfPar', 'msgfParModificationList'),
    function(object, value) {
        object@modification <- value
        object
    }
)
#' @rdname msgfPar_get_set-methods
#' 
setMethod(
    'nMod', 'msgfPar',
    function(object){
        object@modification@nMod
    }
)
#' @rdname msgfPar_get_set-methods
#' 
#' @name nMod<-
#' 
setReplaceMethod(
    'nMod', c('msgfPar', 'numeric'),
    function(object, value) {
        object@modification@nMod <- value
        object
    }
)