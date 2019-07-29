.onAttach <- function (libname, pkgname) {

  packageStartupMessage('SAFE package reminder: Please set SAFE_data_dir using set_safe_dir()')
  options(safedata.verbose=TRUE)

}