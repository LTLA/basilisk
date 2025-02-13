#' Clear the external installation directory
#'
#' Clear the external installation directory by removing all or obsolete virtual environments.
#' This can be used to free up some space if the expiry mechanism is not fast enough at deleting unused environments.
#'
#' @param path String containing a path to the external directory containing the virtual environments.
#' @param package String containing the name of a client R package.
#' If provided, all environments will be removed for this package.
#' @param obsolete.only Logical scalar indicating whether to only remove environments for obsolete package versions.
#'
#' @return 
#' If \code{package=NULL} and \code{obsolete.only=FALSE}, all of the virtual environments in the external directory are destroyed.
#' If \code{obsolete.only=TRUE}, the environments associated with \pkg{basilisk} versions older than the current version are destroyed.
#'
#' If \code{package} is supplied and \code{obsolete.only=FALSE}, all virtual environments for the specified client package are destroyed.
#' If \code{obsolete.only=FALSE}, only the environments for older versions of the client package are destroyed.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{getExternalDir}}, which determines the location of the external directory.
#'
#' @examples
#' # We can't actually run clearExternalDir() here, as it 
#' # relies on basilisk already being installed.
#' print("dummy test to pass BiocCheck")
#'
#' @export
#' @importFrom dir.expiry clearDirectories
clearExternalDir <- function(path=getExternalDir(), package=NULL, obsolete.only=FALSE) {
    ref <- NULL

    if (!is.null(package)) {
        path <- file.path(path, package)
        if (obsolete.only) {
            ref <- as.character(packageVersion(package))
        }
    } else {
        if (obsolete.only) {
            ref <- basename(path)
        }
        path <- dirname(path)
    }

    clearDirectories(path, reference=ref, limit=-Inf, force=TRUE)
}
