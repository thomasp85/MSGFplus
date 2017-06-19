#' Create terminal compatible strings of input and output names
#' 
#' @param rawfile The location of the spectrum file
#' 
#' @param savename The location where the results should be saved
#' 
#' @return A character vector that is compatible with the OS that can be 
#' concatenated with the MS-GF+ call
#' 
#' @noRd
#' 
createFileCall <- function(rawfile, savename){
    paste0('-s ', shQuote(rawfile), ' -o ', shQuote(savename))
}

#' Download the MS-GF+ jar file
#' 
#' This function downloads and extracts MS-GF+ into the package directory
#' 
#' @noRd
#' @importFrom utils download.file unzip
#' 
getMSGFplus <- function() {
    zipFile <- tempfile()
    msgfLocation <- file.path(system.file(package='MSGFplus'), 'MSGFPlus')
    dir.create(msgfLocation)
    download.file('http://proteomics.ucsd.edu/Software/MSGFPlus/MSGFPlus.20140630.zip', destfile = zipFile)
    unzip(zipfile = zipFile, exdir = msgfLocation)
}