NULL

.onAttach <- function(libname, pkgname) {
  # this data package is not supposed to be attached
  #
  # when the user attempts to attach it with library() or similar, we will
  # output a warning and unload the package again
  #
  # however, doing so will result in an warning during installation, so we need
  # to skip this
  if (!is.na(Sys.getenv("R_TESTS", unset = NA)) || identical(Sys.getenv("R_PACKAGE_NAME"), pkgname)) return()

  rlang::warn(c(
    "Attaching data package `humanEcologyGrid` directly is discouraged",
    "i" = "please prefer to use `humanEcologyGrid::grid_cells()` etc. directly",
    "i" = "instead of loading the package with `library(humanEcologyGrid)`",
    "",
    "i" = "this helps clarify the usage and avoid naming conflicts"
  ))
}
