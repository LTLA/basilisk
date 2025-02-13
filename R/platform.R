#' Find the operating system or architecture.
#'
#' Indicate whether we are on Windows or MacOSX.
#' For MacOSX and Linux, we can also determine if we are on an x86-64 or Arm-based architecture.
#'
#' @return Logical scalar indicating whether we are on the specified OS and/or architecture.
#'
#' @author Aaron Lun
#'
#' @examples
#' isWindows()
#' isMacOSX()
#' isLinux()
#'
#' @rdname platform
#' @export
isWindows <- function() {
    .Platform$OS.type=="windows" 
}

#' @export
#' @rdname platform
isMacOSX <- function() {
    Sys.info()[["sysname"]] == "Darwin"
}

#' @export
#' @rdname platform
isMacOSXArm <- function() {
    isMacOSX() && grepl("^arm", Sys.info()[["machine"]])
}

#' @export
#' @rdname platform
isLinux <- function() {
    Sys.info()[["sysname"]] == "Linux"
}

#' @export
#' @rdname platform
isLinuxAarch64 <- function() {
    isLinux() && Sys.info()[["machine"]] == "aarch64"
}
