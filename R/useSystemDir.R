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
