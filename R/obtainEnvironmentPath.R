#' Obtain the environment path
#'
#' Obtain a path to a Conda environment, lazily installing it if necessary.
#'
#' @param env A string or \linkS4class{BasiliskEnvironment} object specifying the environment.
#'
#' @return String containing the path to an instantiated Conda environment.
#' It will also lazily create the environment if \code{useSystemDir()} returns \code{FALSE}
#' and the environment does not already exist.
#'
#' @details
#' A lot of this function used to belong to \code{\link{setupBasiliskEnv}}.
#' I rolled it out into a separate entity so that \code{\link{setupBasiliskEnv}} only needs to focus on creating the environment.
#' It should not worry about whether or not the environment needs to be created, which is rather context-dependent anyway.
#' Indeed, such decisions only really need to be made during lazy installation and not in other contexts.
#' 
#' @author Aaron Lun
#'
#' @importFrom dir.expiry lockDirectory unlockDirectory touchDirectory
#' @importFrom utils packageVersion
#' @rdname INTERNAL_obtainEnvironmentPath
.obtainEnvironmentPath <- function(env) {
    if (is.null(env)) {
        installConda()
        envpath <- getCondaDir()

    } else {
        envname <- .getEnvName(env)
        pkgname <- .getPkgName(env)

        if (is.null(pkgname)) {
            envpath <- envname

        } else if (useSystemDir()) {
            envpath <- file.path(.get_env_system_dir(pkgname, installed=TRUE), envname)
            if (!file.exists(envpath)) {
                stop(sprintf("environment '%s' should have been created during '%s' installation", envname, pkgname))
            }

        } else {
            # Make sure that conda is installed - do this first. This also
            # applies a lock to ensure that we wait for any concurrently
            # running Conda installations to finish.
            installConda()

            # Decide whether we want to destroy things.
            do.destroy <- destroyOldVersions()

            # Applying a new shared lock to protect the current use-case from
            # deletion by other processes clearing out stale installations.
            exdir <- getExternalDir()
            exlck <- lockExternalDir(exdir, exclusive=FALSE)
            on.exit(unlockExternalDir(exlck, clear=do.destroy))

            envdir <- file.path(exdir, pkgname, packageVersion(pkgname))
            envpath <- file.path(envdir, envname)

            # Locking the environment directory; this ensures we will wait for
            # any concurrently running installations to finish. Do NOT assign
            # the existence of envpath to a variable for re-use in the
            # conditional below. We want to recheck existance just in case the
            # directory was created after waiting to acquire the lock.
            lck <- lockDirectory(envdir, exclusive=!file.exists(envpath))
            on.exit(unlockDirectory(lck, clear=do.destroy), add=TRUE, after=FALSE)

            if (!file.exists(envpath)) {
                setupBasiliskEnv(envpath, 
                    packages=.getPackages(env), 
                    channels=.getChannels(env),
                    pip=.getPipPackages(env),
                    paths=file.path(getSystemDir(pkgname, installed=TRUE), .getPipPaths(env))) # package must already be installed to get to this point.
            }

            # Touching both the individual package directory _and_ the conda
            # directory on successful acquisition of the path.
            touchDirectory(envdir)
            touchDirectory(exdir)
        }
    }

    envpath
}
