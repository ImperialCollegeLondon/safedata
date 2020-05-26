# Test environments

## win-builder

### release

* using log directory 'd:/RCompile/CRANguest/R-release/safedata.Rcheck'
* using R version 4.0.0 (2020-04-24)
* using platform: x86_64-w64-mingw32 (64-bit)

### devel

* using log directory 'd:/RCompile/CRANguest/R-devel/safedata.Rcheck'
* using R Under development (unstable) (2020-05-22 r78545)
* using platform: x86_64-w64-mingw32 (64-bit)

## Ubuntu (Travis/Xenial)

### release

$ Rscript -e 'sessionInfo()'
R version 4.0.0 (2020-04-24)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 16.04.6 LTS

### devel

$ Rscript -e 'sessionInfo()'
R Under development (unstable) (2020-05-26 r78575)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 16.04.6 LTS

## Mac OS (Travis)

### release

$ Rscript -e 'sessionInfo()'
R version 4.0.0 (2020-04-24)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS High Sierra 10.13.6

# R CMD CHECK results

There were no ERRORs or WARNINGS and 1 NOTE in both release and devel using --as-cran.

    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'David Orme <d.orme@imperial.ac.uk>'
    
    New submission
    
    Possibly mis-spelled words in DESCRIPTION:
      Zenodo (14:5)

Zenodo is spelled correctly.


