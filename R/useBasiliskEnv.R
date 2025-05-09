#' Use \pkg{basilisk} environments
#'
#' Use \pkg{basilisk} environments for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envpath String containing the path to the \pkg{basilisk} environment to use. 
#' @param full.activation Deprecated and ignored.
#' 
#' @return 
#' The function will attempt to load the specified \pkg{basilisk} environment into the R session,
#' possibly with the modification of some environment variables (see Details).
#'
#' A function is invisibly returned that accepts no arguments and resets all environment variables to their original values prior to the \code{useBasiliskEnv} call.
#'
#' @details
#' \code{useBasiliskEnv} will load the Python binary at \code{envpath} (or specifically, its shared library) into the current R session via \pkg{reticulate}.
#' Users can then use, e.g., \code{\link[reticulate]{import}} to access functionality in the Python packages installed in the virtual environment.
#'
#' To ensure that the correct packages are used, \code{useBasiliskEnv} will unset environment variables like \code{PYTHONPATH} and \code{PYTHONHOME}.
#' These can be restored by running the function returned by \code{useBasiliskEnv}.
#' 
#' It is unlikely that developers should ever need to call \code{\link{useBasiliskEnv}} directly.
#' Rather, this interaction should be automatically handled by \code{\link{basiliskStart}}.
#'
#' @author Aaron Lun
#' 
#' @examples
#' if (.Platform$OS.type != "windows") {
#'   tmploc <- file.path(tempdir(), "my_package_A")
#'   if (!file.exists(tmploc)) {
#'       setupBasiliskEnv(tmploc, c('pandas==2.2.3'))
#'   }
#' 
#'   # This may or may not work, depending on whether a Python instance
#'   # has already been loaded into this R session.
#'   try(useBasiliskEnv(tmploc))
#' }
#'
#' @seealso
#' \code{\link{basiliskStart}}, for how these \pkg{basilisk} environments should be used.
#'
#' @export
#' @importFrom reticulate use_virtualenv py_config
useBasiliskEnv <- function(envpath, full.activation=NA) {
    envpath <- normalizePath(envpath, mustWork=TRUE)

    # Unset these so we don't pick up packages from other locations.
    collected <- list()
    for (env in c("PYTHONPATH", "PYTHONHOME")) {
        out <- Sys.getenv(env, NA)
        if (!is.na(out)) {
            collected[[env]] <- out
            Sys.unsetenv(env)
        }
    }

    use_virtualenv(envpath, required=TRUE)

    # use_virtualenv doesn't actually cause Python to be loaded immediately, 
    # so we use py_config() to forcibly load it and prevent other Python
    # instances from filling the slot.
    py_config() 

    invisible(function() {
        if (length(collected)) {
            do.call(Sys.setenv, collected)
        }
    })
}

#' @importFrom reticulate py_config 
.same_as_loaded <- function(envpath) {
    # - Checking whether we're the same as the existing python instance, which
    #   would indicate that we correctly loaded `envpath`. The normalization of
    #   the environment ensures that we're matching the path in useBasiliskEnv.
    # - Don't normalize the binary path itself as this is a symlink.
    # - Set winslash= to match py_config()'s use of forward slashes. 
    expected <- getPythonBinary(normalizePath(envpath, winslash="/"))
    actual <- py_config()$python
    identical(expected, actual)
}
