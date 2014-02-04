.onLoad <- function(libname, pkgname) {
    javaVersion <- system2('java', '-version', stderr=T, stdout=T)
    if(length(javaVersion) == 0) {
        warning('Java was not found on your system. Java is required for MS-GF+ to work.')
    } else {
        javaVersion <- javaVersion[grepl('java version', javaVersion)]
        as.numeric(sub('.*\"\\d\\.(\\d).*', '\\1', javaVersion))
        if(as.numeric(sub('.*\"\\d\\.(\\d).*', '\\1', javaVersion)) < 7) {
            warning('Java need to be at least version 1.7 for MS-GF+ to work. Please upgrade.')
        }
    }
    printMSGFLicense()
}