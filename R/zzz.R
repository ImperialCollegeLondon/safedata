.onLoad <- function (libname, pkgname) {
  #' Function to run on import of package \code{safer}

  # check whether data cache exists, if not, create it
  # *not sure where to put the SAFE cache right now - should it go inside the
  # package (e.g. data/) or should it be user-defined?
  # packageStartupMessage('Checking for SAFE cache...')
}
