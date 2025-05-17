# library(testthat); library(basilisk); source("setup.R"); source("test-setupBasiliskEnv.R"); 

target <- file.path(client.dir, "thingo")

test_that("setupBasiliskEnv refuses to work without all specified versions", {
    basilisk:::.unlink2(target)
    expect_error(setupBasiliskEnv(target, "numpy"), "versions must be explicitly specified")
    expect_error(setupBasiliskEnv(target, "numpy>=10"), "versions must be explicitly specified")
    expect_error(setupBasiliskEnv(target, "numpy<=10"), "versions must be explicitly specified")
})

test_that("setupBasiliskEnv obtains the correct version of the packages", {
    basilisk:::.unlink2(target)
    setupBasiliskEnv(target, c(test.pandas, test.pandas.deps))
    incoming <- basilisk:::.basilisk_freeze(target)
    expect_true(test.pandas %in% incoming)
    expect_true(all(test.pandas.deps %in% incoming))

    basilisk:::.unlink2(target)
    setupBasiliskEnv(target, c(old.pandas, old.pandas.deps))
    incoming <- basilisk:::.basilisk_freeze(target)
    expect_true(old.pandas %in% incoming)
    expect_true(all(old.pandas.deps %in% incoming))

    # Listers also work as expected.
    expect_match(listPythonVersion(target), "^3")
    info <- listPackages(target)
    expect_true("pandas" %in% info$package)
})

test_that("setupBasiliskEnv will install Python 2.7 if requested", {
    basilisk:::.unlink2(target)
    setupBasiliskEnv(target, "python=2.7.18")
    env.py <- basilisk::getPythonBinary(target)
    py.ver <- system2(env.py, "--version", stderr=TRUE, stdout=TRUE)
    expect_match(py.ver, "2\\.7")

    # Same if we use the lister.
    expect_match(listPythonVersion(target), "^2\\.7")
})

test_that("setupBasiliskEnv works with local packages", {
    basilisk:::.unlink2(target)
    setupBasiliskEnv(target, packages=character(0), paths=system.file("example", "inst", "test_dummy", package="basilisk"))
    incoming <- basilisk:::.basilisk_freeze(target)
    expect_true("test_dummy==0.1" %in% incoming)
})

test_that("setupBasiliskEnv destroys directory on error", {
    basilisk:::.unlink2(target)
    expect_error(setupBasiliskEnv(target, package="WHHEEEEEEEEEEEEEEEEEE==0.0.1"))
    expect_false(file.exists(target))
})

resetenv <- function(var, old) {
    if (is.na(old)) {
        Sys.unsetenv(var)
    } else {
        names(old) <- var
        do.call(Sys.setenv, as.list(old))
    }
}

test_that("setupBasiliskEnv responds to overrides", {
    host <- suppressMessages(install_python(basilisk::defaultPythonVersion))

    version.components <- strsplit(basilisk::defaultPythonVersion, "\\.")[[1]]
    customvar <- paste(c("BASILISK_CUSTOM_PYTHON", version.components), collapse="_")
    names(host) <- customvar

    old <- Sys.getenv(customvar, NA)
    do.call(Sys.setenv, as.list(host))
    on.exit(resetenv(customvar, old), add=TRUE, after=TRUE)

    # Not actually sure how to check that it used the environment variable... oh well.
    basilisk:::.unlink2(target)
    setupBasiliskEnv(target, c(test.pandas, test.pandas.deps))
    expect_true(file.exists(target))

    # But we can for sure disable it and check that we get an error.
    Sys.unsetenv(customvar)
    oldp <- Sys.getenv("BASILISK_NO_PYENV", NA)
    Sys.setenv(BASILISK_NO_PYENV=1)
    on.exit(resetenv("BASILISK_NO_PYENV", oldp), add=TRUE, after=TRUE)
    expect_error(setupBasiliskEnv(target, c(test.pandas, test.pandas.deps)), customvar)
})
