# Test environments

## win-builder

### release

* using log directory 'd:/RCompile/CRANguest/R-release/safedata.Rcheck'
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-w64-mingw32 (64-bit)

### devel

* using log directory 'd:/RCompile/CRANguest/R-devel/safedata.Rcheck'
* using R Under development (unstable) (2020-07-29 r78934)
* using platform: x86_64-w64-mingw32 (64-bit)

## Ubuntu (Travis/Xenial)

### release

$ Rscript -e 'sessionInfo()'
R version 4.0.0 (2020-04-24)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 16.04.6 LTS

### devel

$ Rscript -e 'sessionInfo()'
R Under development (unstable) (2020-07-31 r78945)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 16.04.6 LTS

## Mac OS (Travis)

### release

$ Rscript -e 'sessionInfo()'
R version 4.0.2 (2020-06-22)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS High Sierra 10.13.6

# R CMD CHECK results

Status OK on all test platforms. 
* 1 Note on win-builder about the spelling of Zenodo.
* 1 Warning on win-builder devel with an HTTP 500 error on an external APO. The service is working - I think this was just transitory.

