#' Set an environment variable
#'
#' Set an environment variable safely, unsetting it if the supplied value is \code{NA}.
#'
#' @param name String containing the name of an environment variable.
#' @param value String containing the value of an environment variable.
#' This can be \code{NA} to unset the variable.
#'
#' @return String containing the value of the variable before running this function;
#' or \code{NA}, if the variable was not set.
#'
#' @author Aaron Lun
#'
#' @export
setVariable <- function(name, value) {
    old <- Sys.getenv(name, NA_character_)
    if (is.na(value)) {
        Sys.unsetenv(name)
    } else {
        X <- list(value)
        names(X) <- name 
        do.call(Sys.setenv, X)
    }
    invisible(old)
}
