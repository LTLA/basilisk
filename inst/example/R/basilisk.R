#' @importFrom basilisk BasiliskEnvironment
env1 <- BasiliskEnvironment("env1", pkgname="son.of.basilisk", packages=c("pandas=2.2.3"))

#' @importFrom basilisk BasiliskEnvironment
env2 <- BasiliskEnvironment("env2", pkgname="son.of.basilisk", packages=c("scikit-learn==1.6.1"))

#' @importFrom basilisk BasiliskEnvironment
env3 <- BasiliskEnvironment("env3", pkgname="son.of.basilisk", packages=character(0), path="test_dummy")
