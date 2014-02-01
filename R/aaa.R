getMSGF <- function() {
    cat('Downloading MS-GF+\n')
    downloadLoc <- R.home(component='library/MSGFplus/inst/msgf.zip')
    download.file(url=http://dl.dropboxusercontent.com/u/2323585/MSGFPlus.zip, destfile=downloadLoc)
    unzip(downloadLoc)
}
checkMSGF <- function() {
    file.exists(R.home(component='library/MSGFplus/inst/MSGFPlus/MSGFPlus.jar'))
}
printMSGFLicense <- function() {
    if (checkMSGF()) {
        writeLines(readLines(R.home(component='library/MSGFplus/inst/MSGFPlus/LICENSE.txt')))
    } else {
        cat('MS-GF+ not installed\n')
    }
}