#' List packages
#'
#' List the set of Python packages (and their version numbers) that are installed in a virtual environment.
#'
#' @inheritParams basiliskStart
#'
#' @author Aaron Lun
#'
#' @return 
#' For \code{listPackages}, a data.frame containing the \code{full}, a versioned package string, and \code{package}, the package name.
#'
#' For \code{listPythonVersion}, a string containing the default version of Python.
#' 
#' @examples
#' tmploc <- file.path(tempdir(), "my_package_A")
#' if (!file.exists(tmploc)) {
#'     setupBasiliskEnv(tmploc, c('pandas=1.4.3'))
#' }
#' listPackages(tmploc)
#' listPythonVersion(tmploc)
#' 
#' @export
listPackages <- function(env) {
    envpath <- obtainEnvironmentPath(env)
    out <- .basilisk_freeze(envpath)
    data.frame(full=out, package=.full2pkg(out), stringsAsFactors=FALSE)
}

.basilisk_freeze <- function(envpath) {
    system2(getPythonBinary(envpath), c("-m", "pip", "list", "--format", "freeze"), stdout=TRUE)
}

.full2pkg <- function(packages) {
    sub("[><=]+.*", "", packages)
}

#' @export
#' @rdname listPackages
listPythonVersion <- function(env) {
    envpath <- obtainEnvironmentPath(env)
    .python_version(envpath)
}

.python_version <- function(dir) {
    py.cmd <- getPythonBinary(dir)
    dump <- system2(py.cmd, "--version", stdout=TRUE, stderr=TRUE)
    pattern <- "^Python "
    sub(pattern, "", dump[grep(pattern, dump)[1]])
}

