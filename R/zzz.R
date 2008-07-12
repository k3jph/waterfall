##
##
##

.First.lib <-
function(libname, pkgname, ...) {
    if (is.R()) {
        library(package = "lattice")
        return(TRUE)
    }
}
