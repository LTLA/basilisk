#' Get the system installation directory
#'
#' Get the system installation directory for a package.
#' This is not entirely trivial as it may be called before the package is installed.
#'
#' @param pkgname String containing the package name.
#' @param installed Logical scalar specifying whether the package is likely to be installed yet.
#'
#' @author Aaron Lun
#'
#' @return String containing the path to the (likely, if \code{installed=FALSE}) installation directory for \code{pkgname}.
#'
#' @examples
#' getSystemDir("basilisk", installed=FALSE)
#' 
#' @export
getSystemDir <- function(pkgname, installed) {
    if (installed) {
        # This is more robust than .libPaths(), which may change
        # between *basilisk* installation and client installation;
        # system.file() should still pull out the correct dir.
        vdir <- system.file(package=pkgname)
    } else {
        # As this is run in configure, system.file() will not work, as pkgname
        # isn't even installled yet! 
        vdir <- file.path(.libPaths()[1], pkgname)
    }
}
