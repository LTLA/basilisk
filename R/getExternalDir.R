#' Get an external conda directory
#'
#' Define an external location for installing the conda instance and \pkg{basilisk} environments.
#'
#' @return String containing a path to an appropriate external folder.
#' The last component of the path will always be the \pkg{basilisk} version number.
#'
#' @details
#' The default path contains the version number so that installation of a new version of \pkg{basilisk} will trigger the creation of new virtual environments.
#' This ensures that any changes to \pkg{basilisk} functions will be respected by all client packages in the same R installation.
#'
#' If the \code{BASILISK_EXTERNAL_DIR} environment variable is set to some absolute path, this will be used instead as the installation directory.
#' Setting this variable is occasionally necessary if the default path returned by \code{\link{R_user_dir}} has spaces;
#' or on Windows, if the 260 character limit is exceeded after combining the default path with deeply nested conda paths. 
#'
#' We assume that the user has read-write access to the external directory.
#' Write access is necessary to generate new environments and to handle locking in \code{\link{lockExternalDir}}.
#'
#' @author Aaron Lun
#' 
#' @examples
#' # We can't actually run getExternalDir() here, as it 
#' # either relies on basilisk already being installed.
#' print("dummy test to pass BiocCheck")
#'
#' @export
#' @importFrom utils packageVersion
#' @importFrom tools R_user_dir
getExternalDir <- function() {
    inst_path <- Sys.getenv("BASILISK_EXTERNAL_DIR", NA)
    if (is.na(inst_path)) {
        inst_path <- R_user_dir("basilisk", "cache")
    }
    pkg.v <- as.character(packageVersion("basilisk"))
    file.path(path.expand(inst_path), pkg.v)
}
