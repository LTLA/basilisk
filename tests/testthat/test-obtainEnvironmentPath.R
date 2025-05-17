# library(testthat); library(basilisk); source("setup.R"); source("test-obtainEnvironmentPath.R")

tmp <- tempfile()
dir.create(tmp)

setVariable <- function(var, val) {
    old <- Sys.getenv(var, NA)
    if (is.na(val)) {
        Sys.unsetenv(var)
    } else {
        names(val) <- var
        do.call(Sys.setenv, as.list(val))
    }
    old
}

old <- setVariable("BASILISK_USE_SYSTEM_DIR", NA)
old.2 <- setVariable("BASILISK_EXTERNAL_DIR", tmp)

test_that("obtainEnvironmentPath works as expected", {
    testpkg <- "basilisk"
    env <- BasiliskEnvironment(envname="test", pkgname=testpkg, packages=test.pandas)
    dummy <- file.path(getExternalDir(), testpkg, "0.0.1")
    dir.create(dummy, recursive=TRUE)
    dir.expiry::touchDirectory(dummy, date=Sys.Date()-100)

    # Omits destruction with NO_DESTROY=1.
    old.d <- setVariable("BASILISK_NO_DESTROY", "1")
    out <- obtainEnvironmentPath(env)
    expect_true(file.exists(out))
    expect_true(file.exists(dummy))
    setVariable("BASILISK_NO_DESTROY", old.d)

    # Otherwise, destruction of older versions works properly.
    old.d <- setVariable("BASILISK_NO_DESTROY", NA)
    out <- obtainEnvironmentPath(env)
    expect_true(file.exists(out))
    expect_false(file.exists(dummy))
    setVariable("BASILISK_NO_DESTROY", old.d)
})

setVariable("BASILISK_USE_SYSTEM_DIR", old)
setVariable("BASILISK_EXTERNAL_DIR", old.2)
