# Test environments

## win-builder

### release

* using log directory 'd:/RCompile/CRANguest/R-release/safedata.Rcheck'
* using R version 3.6.3 (2020-02-29)
* using platform: x86_64-w64-mingw32 (64-bit)

### devel

* using log directory 'd:/RCompile/CRANguest/R-devel/safedata.Rcheck'
* using R version 4.0.0 alpha (2020-03-26 r78078)
* using platform: x86_64-w64-mingw32 (64-bit)

## Ubuntu (Travis/Xenial)

### release

* using log directory ‘/home/travis/build/ImperialCollegeLondon/safedata/safedata.Rcheck’
* using R version 3.6.2 (2017-01-27)
* using platform: x86_64-pc-linux-gnu (64-bit)

### devel

* installation problems with R in devel: knitr and units

## Mac OS 

### release

* using R version 3.6.2 (2019-12-12)
* using platform: x86_64-apple-darwin15.6.0 (64-bit)

# R CMD CHECK results

There were no ERRORs or WARNINGS and 1 NOTE in both release and devel using --as-cran.

    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'David Orme <d.orme@imperial.ac.uk>'
    
    New submission
    
    Possibly mis-spelled words in DESCRIPTION:
      Zenodo (14:5)

Zenodo is spelled correctly.


