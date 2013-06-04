# TODO: Add comment
# 
# Author: Thomas
###############################################################################


setGeneric(
		'isLoaded',
		def=function(object){standardGeneric('isLoaded')}
)
setGeneric(
		'getMSGFpar',
		def=function(object){standardGeneric('getMSGFpar')}
)
setGeneric(
		'database',
		def=function(object){standardGeneric('database')}
)
setGeneric(
		'database<-',
		def=function(object, value){standardGeneric('database<-')}
)
setGeneric(
		'checkResFile',
		def=function(object){standardGeneric('checkResFile')}
)
setGeneric(
		'analysisSoftwareVersion',
		def=function(object){standardGeneric('analysisSoftwareVersion')}
)
setGeneric(
  'runMSGF',
  def=function(object, ...){standardGeneric('checkResFile')}
)