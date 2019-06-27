.onLoad <- function (libname, pkgname) {
  #' Function to run on import of package \code{safer}

  packageStartupMessage('SAFE package reminder: Please set SAFE_data_dir using setSafeDir()')
}