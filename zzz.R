.onLoad <- function(libname, pkgname) {
    if (!checkMSGF()) {
        getMSGF()
        cat('Done\n\n')
        printMSGFLicense()
    }
}