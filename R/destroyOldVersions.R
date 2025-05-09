#' Destroy old versions?
#' 
#' Should we destroy old environments for \pkg{basilisk} client packages?
#'
#' @details
#' The default value is \code{TRUE}, in order to save some disk space.
#' This can be changed by setting \code{BASILISK_NO_DESTROY} environment variable to \code{"1"}.
#' 
#' @return Logical scalar providing an answer to the above.
#'
#' @author Aaron Lun
#'
#' @seealso 
#' \code{\link{obtainEnvironmentPath}}, which removes old environments as a side-effect.
#' 
#' @export
destroyOldVersions <- function() {
    !identical(Sys.getenv("BASILISK_NO_DESTROY", NA), "1")
}
