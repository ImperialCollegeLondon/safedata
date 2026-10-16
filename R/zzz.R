.onAttach <- function(libname, pkgname) {
    # nolint required
    packageStartupMessage(
        "safedata package reminder: Please set safedata ",
        "directory using set_safedata_dir()"
    )
    options(safedata.verbose = TRUE)
}
