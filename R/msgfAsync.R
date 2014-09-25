#' @include aaa.R
#' @include generics.R
#' 
NULL

#' A class referencing an asynchronous execution of MS-GF+
#' 
#' @description Objects of this class contains a reference to an asynchronous running MS-GF+
#' process and can be used to query the state of the process, and import the 
#' results if the process has finished. Instances of this class are created when
#' the runMSGF() method is called with \code{async=TRUE} on an msgfPar object. 
#' 
#' Unlike regular runMSGF() calls, this does not support batch mode, meaning
#' that if a more than one raw file is supplied, all but the first are ignored
#' with a warning.
#' 
#' In order to insulate instances of this class from being corrupted (thus
#' loosing the reference to the process), all slots are functions and should be
#' queried as such if needed.
#' 
#' @slot status Returns the status of the MS-GF+ process; either 'Running' or
#' 'Done'.
#' 
#' @slot resultFile Returns the location of the result file from the MS-GF+ 
#' analysis. WARNING: Checking for the existence of this file is not a safe way 
#' to determine the status of the process, as the file gets written to 
#' continuously.
#' 
#' @examples
#' \dontrun{
#' parameters <- msgfPar(
#'                       database=system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'),
#'                       tolerance='20 ppm',
#'                       instrument='TOF',
#'                       enzyme='Lys-C'
#'                      )
#' asyncMSGF <- runMSGF(parameters, 'file1.mzML', async=TRUE)
#' while(!running(asyncMSGF)){
#'     Sys.sleep(1)
#' }
#' results <- import(asyncMSGF)
#' }
#' 
setClass(
    Class='msgfAsync',
    slots=list(
        status='function',
        resultFile='function'
        )
    )

#' Initialize method for msgfAsync
#' 
#' The initialization method takes care of creating a shared closure for the two
#' slot functions so that the information is shared but only readable. In this 
#' way the user has no way of corrupting the information.
#' 
#' @param .Object The msgfAsync object to be created
#' 
#' @param checkfile The path to the file that is going to be created by the end
#' of the process
#' 
#' @param resultfile The path to the file containing the results of the analysis
#' 
#' @return An msgfAsync object
#' 
#' @noRd
#' 
setMethod('initialize', 'msgfAsync',
          function(.Object, checkfile, resultfile) {
              hasEnded <- FALSE
              .Object@status <- function() {
                  if(hasEnded) return('Done')
                  
                  if(file.exists(checkfile)) {
                      hasEnded <<- TRUE
                      file.remove(checkfile)
                      return('Done')
                  } else {
                      return('Running')
                  }
              }
              .Object@resultFile <- function() {
                  return(resultfile)
              }
              .Object
          }
          )

#' @describeIn msgfAsync Check whether the MS-GF+ process is still running
#' 
#' @param object An msgfAsync object
#' 
#' @return \code{running(object)} Returns a logical indicating if the process
#' is running
#' 
setMethod('running', 'msgfAsync',
          function(object) {
              object@status() == 'Running'
          }
          )

#' @describeIn msgfAsync Check whether the MS-GF+ process is finished
#' 
#' @return \code{finished(object)} Returns a logical indicating if the process
#' is finished
#' 
setMethod('finished', 'msgfAsync',
          function(object) {
              object@status() == 'Done'
          }
)

#' @describeIn msgfAsync Import the result of the asynchronous MS-GF+ process
#' 
#' @return \code{import(object)} Returns an mzID object or NULL if the process
#' is still running. Throws an error if the process is finished but the result
#' file doesn't exist.
#' 
setMethod('import', 'msgfAsync',
          function(object) {
              if(finished(object)) {
                  if(file.exists(object@resultFile())) {
                      mzID(object@resultFile())
                  } else {
                      stop('Process done but result file is missing')
                  }
              } else {
                  NULL
              }
          }
)