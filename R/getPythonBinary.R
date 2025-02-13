#' Get Python binary paths
#'
#' @param envpath String containing the path to a virtual environment.
#'
#' @return String containing the path to the Python executable inside \code{envpath}.
#'
#' @details
#' This code is largely copied from \pkg{reticulate},
#' and is only present here as they do not export these utilities for general consumption.
#'
#' @author Aaron Lun
#'
#' @examples
#' getPythonBinary("foo/bar")
#'
#' @export
#' @rdname getBinaries
getPythonBinary <- function(envpath) {
    # Copied from reticulate::use_virtualenv.
    suffix <- if (isWindows()) "Scripts/python.exe" else "bin/python"
    file.path(envpath, suffix)
}

