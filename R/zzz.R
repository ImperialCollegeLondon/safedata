.onAttach <- function(libname, pkgname) { # nolint required
    packageStartupMessage(
        "SAFE package reminder: Please set SAFE data ",
        "directory using set_safedata_dir()"
    )
    options(safedata.verbose = TRUE)
}
