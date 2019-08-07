.onAttach <- function (libname, pkgname) {

  packageStartupMessage('SAFE package reminder: Please set SAFE data directory using set_safe_dir()')
  options(safedata.verbose=TRUE)

}