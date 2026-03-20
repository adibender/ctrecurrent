.onLoad <- function(libname, pkgname) {
  if (getRversion() >= "2.5.1") {
    utils::globalVariables(".")
  }
  invisible()
}
