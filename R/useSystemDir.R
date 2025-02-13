#' Use the R system directory?
#'
#' Should we use the R system directory for installing \pkg{basilisk}'s Conda instance (or client environments)?
#'
#' @details
#' The default value is \code{FALSE} to avoid problems with position-dependent code in packaged binaries.
#' This can be changed by setting \code{BASILISK_USE_SYSTEM_DIR} environment variable to \code{"1"}.
#' 
#' @return Logical scalar providing an answer to the above.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{installConda}} and \code{\link{getCondaDir}}, where this function is used.
#'
#' @export
useSystemDir <- function() {
    identical(Sys.getenv("BASILISK_USE_SYSTEM_DIR", NA), "1")
}

#' Destroy old versions?
#' 
#' Should we destroy old installations of Conda from previous versions of \pkg{basilisk} 
#' (or old environment installations, for \pkg{basilisk} client packages)?
#'
#' @details
#' The default value is \code{TRUE}, in order to save some hard drive space.
#' This can be changed by setting \code{BASILISK_NO_DESTROY} environment variable to \code{"1"}.
#' 
#' @return Logical scalar providing an answer to the above.
#'
#' @author Aaron Lun
#'
#' @seealso 
#' \code{\link{installConda}}, where this function is used.
#'
#' \code{\link{clearObsoleteDir}}, which may be triggered by this function.
#' 
#' @export
destroyOldVersions <- function() {
    !identical(Sys.getenv("BASILISK_NO_DESTROY", NA), "1")
}

#' Don't install a fallback version of R
#'
#' By default, \code{\link{installConda}} will install a conda-managed R in a new environment,
#' to provide a fallback for \code{GLIBCXX}-mismatch errors in C++-dependent Python packages like \pkg{scipy}.
#' This cost can be avoided if the version of the C++ standard library used by R is known to be compatible with client environments. 
#'
#' @details
#' The default value is \code{FALSE} in order to provide a fallback when clients observe \code{GLIBCXX} errors.
#' This can be changed by setting the \code{BASILISK_NO_FALLBACK_R} environment variable to \code{"1"}.
#'  
#' @return Logical scalar providing an answer to the above.
#'
#' @author Aaron Lun
#'
#' @seealso 
#' \code{\link{installConda}}, where this function is used.
#'
#' @export
noFallbackR <- function() {
    identical(Sys.getenv("BASILISK_NO_FALLBACK_R", NA), "1")
}
