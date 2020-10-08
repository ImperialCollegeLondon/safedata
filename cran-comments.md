# Test environments

## win-builder

### release

* using log directory 'd:/RCompile/CRANguest/R-release/safedata.Rcheck'
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-w64-mingw32 (64-bit)

### devel

* using log directory 'd:/RCompile/CRANguest/R-devel/safedata.Rcheck'
* using R Under development (unstable) (2020-10-05 r79298)
* using platform: x86_64-w64-mingw32 (64-bit)

## Ubuntu (Travis/Xenial)

### release

$ Rscript -e 'sessionInfo()'
R version 4.0.2 (2020-06-22)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 18.04.5 LTS

### devel

$ Rscript -e 'sessionInfo()'
R Under development (unstable) (2020-10-05 r79298)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 18.04.5 LTS

## Mac OS (Travis)

### release

$ Rscript -e 'sessionInfo()'
R version 4.0.2 (2020-06-22)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS High Sierra 10.13.6

# R CMD CHECK results

Status OK on all test platforms. 
* 1 Note on win-builder devel:
    URL: https://imperialcollegelondon.github.io/safedata (moved to https://imperialcollegelondon.github.io/safedata/)

