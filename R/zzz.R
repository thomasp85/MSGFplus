# from http://stackoverflow.com/a/34031214/470769
Sys.which2 <- function(cmd) {
    stopifnot(length(cmd) == 1)
    if (.Platform$OS.type == "windows") {
        suppressWarnings({
            pathname <- shell(sprintf("where %s 2> NUL", cmd), intern=TRUE)[1]
        })
        if (!is.na(pathname)) return(stats::setNames(pathname, cmd))
    }
    Sys.which(cmd)
}

.javaExecutable <- function() Sys.which2("java")



.onLoad <- function(libname, pkgname) {
    javaVersion <- system2(.javaExecutable(), '-version', stderr=TRUE, stdout=TRUE)
    if(length(javaVersion) == 0) {
        warning('Java was not found on your system. Java is required for MS-GF+ to work.')
    } else {
        if(as.numeric(sub('.*\"\\d\\.(\\d).*', '\\1', javaVersion[1])) < 7) {
            warning('Java need to be at least version 1.7 for MS-GF+ to work. Please upgrade.')
        }
    }
}
