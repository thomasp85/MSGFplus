#' Create terminal compatible strings of input and output names
#' 
#' @param rawfile The location of the spectrum file
#' 
#' @param savename The location where the results should be saved
#' 
#' @return A character vector that is compatible with the OS that can be concatenated with the MS-GF+ call
#' 
#' @noRd
#' 
createFileCall <- function(rawfile, savename){
    if(Sys.info()["sysname"] == 'Windows'){
        rawfile <- paste0('\"', rawfile, '\"')
        savename <- paste0('\"', savename, '\"')
    } else {
        rawfile <- gsub(' ', '\\ ', rawfile, fixed=T)
        savename <- gsub(' ', '\\ ', savename, fixed=T)
    }
    paste0('-s ', rawfile, ' -o ', savename)
}

#' Print the MS-GF+ license
#' 
#' This function pipes the content of the MS-GF+ LICENCE file into the stdin of R
#' 
#' @noRd
#' 
printMSGFLicense <- function() {
    license <- system.file(package='MSGFplus', 'MSGFPlus', 'LICENSE.txt')
    if(license != '') {
        cat('                 -=<( MS-GF+ License )>=-\n\n')
        writeLines(readLines(license))
    }
}