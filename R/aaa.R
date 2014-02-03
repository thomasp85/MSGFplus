#' Create terminal compatible strings of input and output names
#' 
#' @param rawfile The location of the spectrum file
#' 
#' @param savename The location where the results should be saved
#' 
#' @return A character vector that is compatible with the OS that can be concatenated with the MS-GF+ call
#' 
createFileCall <- function(rawfile, savename){
    if(Sys.info()["sysname"] == 'Windows'){
        rawfile <- paste0('\"', rawfile, '\"')
        savename <- paste0('\"', savename, '\"')
    } else {
        rawfile <- gsub(' ', '\\ ', rawfile, fixed=T)
        savename <- gsub(' ', '\\ ', savename, fixed=T)
    }
    paste0('-s ', rawfile, ' -o ', savename, ' -d', database)
}

printMSGFLicense <- function() {
    writeLines(readLines(R.home(component='library/MSGFplus/inst/MSGFPlus/LICENSE.txt')))
}