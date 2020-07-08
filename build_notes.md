# Building and releasing safedata

This repository and package uses a number of systems that you need to be aware of if you plan to work with the codebase:


* The repository has (now) been set up to use the git flow system for controlling development and releases across branches.
* Documentation for the package is maintained using `roxygen` and `pkgdown`.
* The repository uses the Travis continuous integration (CI) system. 

These notes provide a brief walk through for these systems and some package specific requirements for releases. Note that the `build_scripts` folder in the repository contains three scripts containing the code blocks used in some of the steps described below. All are set up to be run from directly inside the `build_scripts` directory.

## Documentation

The `roxygen` package , all of the package documentation is located in one of three places:

1. Markup inside the `R` source files - there are blocks of comments starting with `#'` that contain all of the documentation that goes into the normal R documentaion (`.Rd`) files.
2. Vignettes, formatted as `Rmarkdown` files in the `vignettes` directory.
3. The `README.md` document.

When the documentation has been changed then the `.Rd` files in `man` and any vignettes can be built automatically. The key command is:

```sh
Rscript -e "devtools::document()"
```

In addition to using `roxygen` to maintain the in-package documentation, `safedata` also uses `pkgdown` to create a documentation website. This needs one additional file  - `_pkgdown.yml` - that is used to structure the website. This file typically only needs updating when new functions are added, as they need to be added into the reference structure.

Rebuilding the website uses two commands:

```sh
Rscript -e " pkgdown::clean_site()"
Rscript -e " pkgdown::build_site()"
```

This removes old files and recreates the website in the `docs` directory. This directory is then automatically used by GitHub Pages to create the package website at:

https://imperialcollegelondon.github.io/safedata/

## Repo branches and `git flow` 

Day to day development happens on the `develop` branch, although you can also create specific `feature` branches for new features.  When you have code that you want to release as a new version then `git flow` is used to create a `release` branch that is used to check and make final changes. That `release` branch is then merged into `master` and tagged as the new release, and also back into `develop` to bring back last minute fixes.

The `master` branch should really only ever see merges in from a `release` branch - you should not work on it directly.

## Travis CI

When commits are pushed to the Github origin then the package is automatically built and checked by Travis:

https://travis-ci.org/github/ImperialCollegeLondon/safedata

This happens on all branches, so day to day commits to `develop` will be built as well as commits to `release` branches and the creation of new tagged versions on `master`.

If you have made changes that you do not want to be built and checked then you can include `[ci skip]`, but the idea is that all changes should be checked so this is typically only used for documentation changes and the like.


## Release cycle

These are the steps needed to release a new version of `safedata`.

### Configure `git`

It is easier if `git` is configured to push new tags along with commits. This essentially just means that new releases can be sent with a single commit, which is simpler and saves Travis from building both the the code commit and then the tagged version. This only needs to be set once.

```sh
set git config --global push.followTags true
```

### Update the example directory

The `safedata` package ships with a zipped `safedata` directory that is used in the code examples. The search functionality in `safedata` uses the live database via the API so, unless you update this example directory, running the search examples may return Zenodo record IDs that are missing from the example directory. This will create checking errors.

The R code below replaces this file with a fresh zipped example directory.

```refresh_example_dir.R
library(safedata)
setwd('../inst/safedata_example_dir/')
path <- "safedata_example_dir"
# create the new directory and add the example file
set_safe_dir(path, create=TRUE)
download_safe_files(1400562, confirm=FALSE)
# remove the old archive
file.remove('safedata_example_dir.zip')
# create the new one from the updated directory
zip('safedata_example_dir.zip', path)
# remove the directory, leaving just the refreshed archive
unlink(path, recursive=TRUE)
```

If course, if someone publishes a new dataset before the release occurs, you may still get an error and have to repeat this step!

### Check the `develop` code.

Releases start from the `develop` branch, with a bunch of commits that you want to release as a new version. Before you do anything, you should check that the current commit in `develop` is building correctly on Travis.

In practice, you have probably just finished tweaking a code file, so you might as well run through the full checking process locally.

```build_and_check.sh
# a) Move into the source directory and update the documents
#    using Roxygen and pkgdown
cd safedata
Rscript -e "devtools::document()"
Rscript -e " pkgdown::clean_site()"
Rscript -e " pkgdown::build_site()"
cd ../

# b) Build the package from the source directory
R CMD BUILD safedata

# c) Identify the version name that just got built from the 
#    package DESCRIPTION file and then check it for CRAN
VERSION=$(sed -n -e '4p' safedata/DESCRIPTION  | cut -d " "  -f 2)
R CMD CHECK --as-cran --run-donttest safedata_$VERSION.tar.gz 
```

Look at the output of `R CMD CHECK` and resolve any issues before moving on. Do not worry about a note saying `Version contains large components` - that is about to be fixed.

### Create the new release branch

The following creates a new candidate branch containing the current `develop` code. You need to specify the upcoming release version number, so for example to release version `1.0.6`:

```sh
git flow release start 1.0.6
```

This will create the `release/1.0.6` branch and check it out.

You should now immediately update the `DESCRIPTION` file to match that version number. In this example, that should mean changing the previous development version number (`-9000` is used to indicate code in development between versions ):

```
Version: 1.0.5-9000
```

to 

```
Version: 1.0.6
```

When you commit that change, the Travis CI process will start for the `release` branch. Travis is configured (see `.travis.yml`) to build the package under R stable on Ubuntu and Mac and R devel on Ubuntu. 

### Submit to `win-builder`

Travis CI does not currently check packages under Windows, but the R Project maintains a Windows test environment that can be used instead. You should build the package on the `release` branch. If everything checked out ok before creating the release, this is just updating the version name. 

You then need to upload that file to `win-builder`. The python script `upload_to_win-builder.py` in `build_scripts` will do this for you - it is simply automating the process of using FTP to upload the current version for checking under both R stable and R devel. Note that `win-builder` communicate by email with the package maintainer (whoever has the `cre` flag in the `authors` section of the `DESCRIPTION` file.

### Wait.

Ideally what happens now is that the build and check process on Travis CI and `win-builder` all pass. You must wait for these checks to complete.

Obviously, if any errors or warnings crop up in the checking process, those should be fixed in the `release` branch. The changes should be committed and pushed to start a new round of Travis CI checking and you will need to rebuild and resubmit to `win-builder`

### Finish the release

Once the `release` branch is passing checks on all platforms, then the candidate release is ready to be released as a version. Using `1.0.6` as the example version number again, the command is:
.
```sh
git flow release finish 1.0.6
```

You will be asked for some commit messages and a new tag comment, which will simply be the version number. You should then be on the `master` branch in a fresh commit tagged with the version number. You can now push this to create the release - if you've set the config described above then a single push will create the commit and tag.

```sh
git push
```


You should now _immediately_ get off the `master` branch, before you accidentally change the files or commit to it, and update the version number in `DESCRIPTION` to indicate that changes are now in the development version of the new release. This is a trivial change, so we can use `[ci skip]` to avoid triggering a Travis build.

```sh
git checkout develop
# Edit DESCRIPTION to e.g. version 1.0.6-9000
git commit -m "Bump develop version [ci skip]" DESCRIPTION
git push
```


### Release to CRAN

You  should now use the outputs from Travis CI and `win-builder` to update the file `cran-comments.md` and the contents of that should be copied into in the comments section of the submission form. The CRAN maintainers expect submitted packages to be functional and fully checked and these notes will help them see that the package has been properly checked.

You can then submit the built package at:
https://cran.r-project.org/submit.html
