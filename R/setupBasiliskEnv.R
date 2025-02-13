#' Set up \pkg{basilisk}-managed environments
#'
#' Set up a virtual environment for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envpath String containing the path to the environment to use. 
#' @param packages Character vector containing the names of PyPI packages to install into the environment.
#' Version numbers must be included.
#' @param channels Character vector containing the names of Conda channels to search.
#' Defaults to the conda-forge repository.
#' @param pip Same as \code{packages}.
#' @param paths Character vector containing absolute paths to Python package directories, to be installed by \code{pip}.
#' 
#' @return 
#' A virtual environment is created at \code{envpath} containing the specified \code{packages}.
#' A \code{NULL} is invisibly returned.
#'
#' @details
#' Developers of \pkg{basilisk} client packages should never need to call this function directly.
#' For typical usage, \code{setupBasiliskEnv} is automatically called by \code{\link{basiliskStart}} to perform lazy installation.
#' Developers should also create \code{configure(.win)} files to call \code{\link{configureBasiliskEnv}},
#' which will call \code{setupBasiliskEnv} during R package installation when \code{BASILISK_USE_SYSTEM_DIR=1}.
#'
#' Pinned version numbers must be present for all desired \code{packages}.
#' This improves predictability and simplifies debugging across different systems.
#' Any \code{=} version specifications will be automatically converted to \code{==}.
#'
#' Additional Python packages can be installed from local directories via the \code{paths} argument.
#' This is useful for \pkg{basilisk} clients vendoring Python packages that are not available in standard repositories.
#' While \code{paths} expects absolute paths for general usage, this will be auto-generated in a package development context -
#' see \code{\link{BasiliskEnvironment}} for details.
#'
#' It is also good practice to explicitly list the versions of the \emph{dependencies} of all desired packages.
#' This protects against future changes in the behavior of your code if the \code{pip} dependency resolver decides to use a different version of a dependency.
#' To identify appropriate versions of dependencies, we suggest:
#' \enumerate{
#' \item Creating a fresh virtual environment with the desired packages, using \code{packages=} in \code{setupBasiliskEnv}.
#' \item Calling \code{\link{listPackages}} on the environment to identify any relevant dependencies and their versions.
#' \item Including those dependencies in the \code{packages=} argument for future use.
#' (It is helpful to mark dependencies in some manner, e.g., with comments, to distinguish them from the actual desired packages.)
#' }
#' The only reason that pinned dependencies are not mandatory is because some dependencies are OS-specific,
#' requiring some manual pruning of the output of \code{\link{listPackages}}.
#'
#' If versions for the desired packages are not known beforehand, developers may use \code{\link{setBasiliskCheckVersions}(FALSE)} before running \code{setupBasiliskEnv}.
#' This instructs \code{setupBasiliskEnv} to create an environment with appropriate versions of all unpinned packages, 
#' which can then be read out via \code{\link{listPackages}} for insertion in the \code{packages=} argument as described above.
#' We stress that this option should \emph{not} be used in any release of the R package, it is a development-phase-only utility.
#'
#' If no Python version is listed, the \code{defaultPythonVersion} is used.
#' However, it is prudent to explicitly list the desired version of Python in \code{packages}, even if this is already version-compatible with the current default.
#' This protects against changes to the Python version in future \pkg{basilisk} versions.
#' Of course, it is possible to specify an entirely different version of Python in \code{packages} by supplying, e.g., \code{"python=3.10"}.
#'
#' @examples
#' if (.Platform$OS.type != "windows") {
#'   tmploc <- file.path(tempdir(), "my_package_A")
#'   if (!file.exists(tmploc)) {
#'       setupBasiliskEnv(tmploc, c('pandas=2.2.3'))
#'   }
#' }
#'
#' @seealso
#' \code{\link{listPackages}}, to list the packages in the virtual environment.
#'
#' @export
#' @aliases defaultPythonVersion
#' @importFrom reticulate install_python virtualenv_install
setupBasiliskEnv <- function(envpath, packages, channels=NULL, pip=NULL, paths=NULL) {
    packages <- sub("([^=])=([^=])", "\\1==\\2", packages)
    packages <- c(packages, pip)
    .check_versions(packages, "[^=<>]==[0-9]")

    success <- FALSE
    unlink2(envpath)
    on.exit(if (!success) unlink2(envpath), add=TRUE, after=FALSE)

    # Determining the Python version to use (possibly from `packages=`).
    if (any(is.py <- grepl("^python=", packages))) {
        version <- sub("^python=+", "", packages[is.py][1])
    } else {
        version <- defaultPythonVersion
    }
    py.cmd <- suppressMessages(install_python(version))

    # Forcing it to be interpreted as a path, not a name.
    if (!grepl("[/\\]", envpath)) {
        envpath <- file.path(getwd(), envpath)
    }

    virtualenv_install(
        envname=envpath, 
        python_version=py.cmd,
        packages=packages,
    )

    if (length(paths)) {
        pip.cmd <- c("-m", "pip", "install", "--no-user")
        for (p in paths) {
            result <- system2(py.cmd, c(pip.cmd, p))
        }
    }

    success <- TRUE
    invisible(NULL)
}

.check_versions <- function(packages, pattern) {
    if (getBasiliskCheckVersions()) {
        if (any(failed <- !grepl(pattern, packages))) {
            stop(paste("versions must be explicitly specified for",
                paste(sprintf("'%s'", packages[failed]), collapse=", ")))
        }
    }
}

#' @export
defaultPythonVersion <- "3.12"
