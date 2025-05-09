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
    # Python 2.7 binaries aren't provided for Arm64, so we'll just skip it.
    skip_on_os(c("mac", "linux"), arch="aarch64")

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

test_that("override selection works correctly", {
    # Unsetting everything so that the tests work correctly.
    namex <- "BASILISK_CUSTOM_PYTHON_3"
    oldx <- Sys.getenv(namex, NA)
    Sys.unsetenv(namex)
    on.exit(resetenv(namex, oldx), add=TRUE, after=TRUE)
    namexy <- "BASILISK_CUSTOM_PYTHON_3_10"
    oldxy <- Sys.getenv(namexy, NA)
    Sys.unsetenv(namexy)
    on.exit(resetenv(namexy, oldxy), add=TRUE, after=TRUE)
    namexyz <- "BASILISK_CUSTOM_PYTHON_3_10_8"
    oldxyz <- Sys.getenv(namexyz, NA)
    Sys.unsetenv(namexyz)
    on.exit(resetenv(namexyz, oldxyz), add=TRUE, after=TRUE)

    expect_null(basilisk:::.check_for_custom_python("3.10.8"))
    expect_null(basilisk:::.check_for_custom_python("3.10"))
    expect_null(basilisk:::.check_for_custom_python("3")) 

    host <- list("FOOBAR")
    names(host) <- namexyz
    do.call(Sys.setenv, host)
    expect_identical(basilisk:::.check_for_custom_python("3.10.8"), "FOOBAR")
    expect_null(basilisk:::.check_for_custom_python("3.10"))
    expect_null(basilisk:::.check_for_custom_python("3")) 

    host <- list("STUFF")
    names(host) <- namexy
    do.call(Sys.setenv, host)
    expect_identical(basilisk:::.check_for_custom_python("3.10.8"), "FOOBAR")
    expect_identical(basilisk:::.check_for_custom_python("3.10"), "STUFF")
    expect_null(basilisk:::.check_for_custom_python("3")) 

    host <- list("BLAH")
    names(host) <- namex
    do.call(Sys.setenv, host)
    expect_identical(basilisk:::.check_for_custom_python("3.10.8"), "FOOBAR")
    expect_identical(basilisk:::.check_for_custom_python("3.10"), "STUFF")
    expect_identical(basilisk:::.check_for_custom_python("3"), "BLAH") 

    Sys.unsetenv(namexyz)
    expect_identical(basilisk:::.check_for_custom_python("3.10.8"), "STUFF")
    expect_identical(basilisk:::.check_for_custom_python("3.10"), "STUFF")
    expect_identical(basilisk:::.check_for_custom_python("3"), "BLAH") 

    Sys.unsetenv(namexy)
    expect_identical(basilisk:::.check_for_custom_python("3.10.8"), "BLAH")
    expect_identical(basilisk:::.check_for_custom_python("3.10"), "BLAH")
    expect_identical(basilisk:::.check_for_custom_python("3"), "BLAH") 
})

test_that("setupBasiliskEnv responds to overrides", {
    host <- suppressMessages(install_python(basilisk::defaultPythonVersion))

    version.components <- strsplit(basilisk::defaultPythonVersion, "\\.")[[1]]
    major <- version.components[[1]]
    minor <- version.components[[2]]
    names(host) <- sprintf("BASILISK_CUSTOM_PYTHON_%s_%s", major, minor)

    old <- Sys.getenv(names(host), NA)
    do.call(Sys.setenv, as.list(host))
    on.exit(unsetenv(names(host), old), add=TRUE, after=TRUE)

    # Not actually sure how to check that it used the environment variable... oh well.
    setupBasiliskEnv(target, c(test.pandas, test.pandas.deps))
    expect_true(file.exists(target))
})
