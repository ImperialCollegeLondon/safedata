# Developing and releasing safedata

This repository and package uses a number of systems that you need to be
aware of if you plan to work with the codebase:

- The repository uses trunk based releases from the `main` branch with
  changes accepted into `main` only through pull requests. We used to
  use the `git flow` approach with release branches from `develop` onto
  `main` but moved to trunk based releases from version 2.0.0.
- Documentation for the package is maintained using `roxygen2` and
  `pkgdown`.
- The code uses the `pre-commit` package to carry out QA checking on
  commits.
- The repository uses GitHub Actions to carry out out checks on pull
  requests to support continuous integration.

These notes provide a brief walk through for these systems and some
package specific requirements for releases.

## Code development

Any new feature or bug fix should start with a new [GitHub
issue](https://github.com/ImperialCollegeLondon/safedata/issues) and
then the creation of a new branch to implement the new code. Make a pull
request from that branch onto `main` with code to add and document the
new code: you will need to request a review and wait for code QA to pass
before the PR can be merged. New releases are made directly from the
`main` branch, following merged pull requests for features and bug fixes
that are going to be included in the new release.

The repository includes a `uvr.toml` configuration file for the
[uvr](https://nbafrank.github.io/uvr/) package manager for R. It is
highly recommended that you download and use `uvr` to manage the
development environment for the package.

- The `uvr sync` command will create and update a local R library
  (`.uvr/library`) containing all of the packages required to develop
  and run the code in the repository.
- The `uv run` command can be used to run specific R script files.
- Alternatively `source .uvr/activate` can be used to activate the
  configured R environment. While the environment is activated, all R
  commands will use the configured package setup.

If you do not use `uvr`, look at the `uvr.toml` file for the package and
development requirements.

The package uses [semantic version numbering](https://semver.org/) and
code on the development branch should use the prerelease token `9000`.
This is explained in more detail below in the description of the release
cycle.

## Package documentation

R packages require a `man` directory containing `Rd` format files that
provide package documentation. Rather than maintain separate files
manually, the `roxygen2` package allows R objects to be documented
directly in the R code files. The `roxygen2` package can then be used to
automatically rebuild and organise the `man` directory when
documentation in the R source files changes.

The `devtools` package provides a wrapper function that can be run to
updates `man` files:

``` sh
Rscript -e "devtools::document()"
```

However, the [`pre-commit` configuration](#the-pre-commit-setup) for
`safedata` includes a step that automatically runs this update. If you
try to commit code without updating the matching `man` files, then the
commit will fail and the documentation will have been updated.

### Vignettes

R packages contain *vignettes* that provide additional documentation
such as background and how to guides. The source content for vignettes
are `Rmarkdown` files in the `vignettes` directory and are set up to use
the `knitr` package to convert the Markdown and code to HTML. The
following tool can be used to rebuild the vignettes:

``` sh
 Rscript -e "devtools::build_articles()" 
```

However, the `R CMD BUILD` process, which is used to generate the source
R package for release also builds the vignettes. This is run during both
local package checking and on GitHub Action checking.

### Website

In addition to using `roxygen2` and `knitr` to maintain the package
documentation and vignettes, `safedata` also uses `pkgdown` to create a
documentation website. This basically builds a complete website and
index around the `man` files as an API reference and the vignettes as
explanatory content (‘articles’).

The configuration for this is in the `_pkgdown.yml` file. This file
typically only needs updating when new functions are added, as they need
to be added into the reference structure. The website will contain an
HTML version of the Rd files and vignettes but also extra content: for
example `README.md` is used to create `index.html` and other markdown
files can be used to provide other content.

You can use the following commands to remove previous website builds and
then recreate the website in the `docs` directory.

``` sh
Rscript -e " pkgdown::clean_site()"
Rscript -e " pkgdown::build_site()"
```

The `docs` directory is *not* part of the continuous integration of the
package as it can be generated automatically from the `Rd` and `Rmd`
files. The `safedata` package therefore includes `docs` in the
`.gitignore` file: you can view the local copy of the website that these
commands generate but it is not managed by git.

The actual [`safedata`
website](https://imperialcollegelondon.github.io/safedata/) can be
rebuilt using the
[`pkgdown::deploy_to_branch()`](https://pkgdown.r-lib.org/reference/deploy_to_branch.html)
command. This automatically builds the website files (as in the local
commands above) and commits them to the `gh-pages` branch in the
repository, which is then deployed to the Github Pages URL for the
`safedata` package:

[\<https://imperialcollegelondon.github.io/safedata/](https://imperialcollegelondon.github.io/safedata/)\>

### Documentation script

For local use, the package and website build steps have been bundled
together in `build_scripts/build_docs.sh`.

## Code formatting and linting

The `safedata` package uses automatic code formatting and linting to
maintain consistent code quality and layout.

- The [`air` formatter](https://posit-dev.github.io/air/) provides fast
  R code formatting, ensuring that changes to the package code follow
  consistent and stable layout.
- The [`jarl` linter](https://jarl.etiennebacher.com/) provides code
  linting: it inspects the code using a set of rules and warns about
  programming errors, bugs and suspicious code patterns.

You do not need to install either of these packages manually, as they
are included in the `pre-commit` setup and so are run automatically on
any new code. However, if you want to run them independently of
`pre-commit` then you need to install them.

## The `pre-commit` setup

`pre-commit` is a Python program that automates running a set of quality
assurance and maintenance checks whenever `git commit` is run. If the
checks fail, the commit will be aborted and you will need to fix the
issues and re-commit your changes. You will need to:

1.  Install [`pre-commit`](https://pre-commit.com/#install)
2.  Install the `pre-commit` configuration for the package from within
    your clone of the `safedata` repository by using:
    `pre-commit install`

The set of checks (‘hooks’) are defined in the `.pre-commit-config.yaml`
configuration file and the current checks are

- Running `roxygen` to check if `Rd` files are up to date.
- Inspecting the R code to check it can be loaded, contains no
  [`browser()`](https://rdrr.io/r/base/browser.html) or
  [`debug()`](https://rdrr.io/r/base/debug.html) commands, and that the
  `DESCRIPTION` file is complete.
- Code formatting and linting using `air` and `jarl`
- Checks for common file issues: sorting, missing line endings,
  oversized files and the inclusion of ‘artifact’ files such as
  `.Rhistory` files.

Note that *some* of these steps may make changes to your files to fix
issues, such as to update `man` files and apply formatting. When this
happens, the `pre-commit` step will fail but you can simply add the
changed files to the commit and re-commit to add the automatic fixes.

## Web mocking

The `safedata` package uses the internet extensively for downloading
package data and metadata. This includes code used in function examples,
vignettes and in [running tests](#package-test-suite). The private
function `safedata:::mock_api()` is used in all of these situations to
turn off connections to the actual internet and instead serve up content
from the `inst/api_data` folder. This ensures that testing and
documentation building is not reliant on internet connections.

If you add new code to the project that requests content from the
internet, you will need to extend the set of mocked URLS in the
`mock_api_urls` list in `R/api_mockr.R` and include links to the
expected content from those links.

At some point, this needs to be made optional, to allow a package build
to also be tested against live content.

## Package test suite

The package contains a test suite used for testing that functions behave
as expected. Currently, this largely checks that network failures are
handled gracefully. Those have to be passing before the package can be
released. The tests are automatically run by `R CMD CHECK` during Github
Actions but can also be run locally using:

``` sh
Rscript -e "devtools::test()"
```

If you want to run a single file of tests, use:

``` sh
Rscript -e "testthat::test_file('tests/testthat/test-local_sdd.R')"
```

## Release cycle

These are the steps needed to release a new version of `safedata`.
GitHub Actions should ensure that `main` is always building correctly
but checking should be repeated during the release process to ensure
that everything is up to date.

### Configure `git`

It is easier if `git` is configured to push new tags along with commits.
This essentially just means that new releases can be sent with a single
commit, which is simpler and saves GitHub Actions from building both the
the code commit and then the tagged version. This only needs to be set
once.

``` sh
set git config --global push.followTags true
```

### Update `main` ready for a release

Once you have decided that the changes added to `main` require a new
release then you also need to create a PR to update the following files:

- Update the `DESCRIPTION` file to increment the version number to the
  new release. For example, a bug patch release would update the line
  `Version: 1.0.5-9000` to `Version: 1.0.6`

- Update `NEWS` to document the changes since the previous version

When this PR is merged into `main`, the GitHub Actions will run the
testing described above.

### Run package build and validation

The GitHub Actions run day-to-day quality checks on the code but a more
stringent set of testing is needed to add a package onto CRAN. The set
of checks has been compiled into a single script in
`build_scripts/build_and_check.sh`. The content of this script is shown
below - you may prefer to run the code step by step and the components
are described below.

``` sh
#!/bin/bash
# Script to build and check safedata

# 0) Show R version being used
R --version

printf "\n------------\n\n"

# 1) Move down into the source directory and update the documents 
#    using Roxygen and pkgdown
cd ../
Rscript -e "devtools::document()"
Rscript -e " pkgdown::clean_site()"
Rscript -e " pkgdown::build_site()"


# 2) Move down again into the parent directory and 
#    build the package from the source directory
cd ../
R CMD BUILD safedata

# 3) Identify the version name that just got built and check it for CRAN
VERSION=$(sed -n -e '4p' safedata/DESCRIPTION  | cut -d " "  -f 2)
R CMD CHECK --as-cran --run-donttest safedata_$VERSION.tar.gz 
```

Two points to note:

1.  The built package and check output are saved in the parent directory
    of the repository, not in the repository itself.
2.  The version built is the one in the *currently checked out branch*!

The key file to look at is `safedata.Rcheck/00check.log`. This contains
a long list of checks applied to the code. Look out for `NOTE`,
`WARNING` and `ERROR` and resolve these issues before moving on. If you
are checking in the `develop` branch then you will see a note saying
`Version contains large components` - that is about to be fixed.

### Checking on different platforms

The GitHub Action build process should now be underway for the `main`
branch. Git Actions is configured (see
`.github/workflows/check-standard.yaml`) to build the package under R
stable on Ubuntu, Mac and Windows and R devel on Ubuntu.

Although Windows is included in the GitHub Actions testing environment,
the R Project also maintains a Windows test environment that can be
used. This needs a built copy of the `main` branch, so run
`build_scripts/build_and_check.sh` again. This should create a newly
built package with the new version number
(e.g. `safedata_1.0.6.tar.gz`). If everything checked out ok before
creating the release, this is really just updating the version name.

You then need to upload that file to `win-builder`. The python script
`build_scripts/upload_to_win-builder.py` will do this for you - it is
simply automating the process of using FTP to upload the current version
for checking under both R stable and R devel. Note that `win-builder`
communicates by email with the package maintainer (whoever has the `cre`
flag in the `authors` section of the `DESCRIPTION` file).

``` sh
python build_scripts/upload_to_win-builder.py
```

### Wait

Ideally what happens now is that the build and check process on GitHub
Actions and `win-builder` all pass. You **must** wait for these checks
to complete!

Obviously, if any errors or warnings crop up in the checking process,
those should be fixed in the `release` branch. The changes should be
committed and pushed to start a new round of GitHub Actions checking and
you will need to rebuild and resubmit to `win-builder`

### Final edits

Before submitting the code to CRAN you should make the following
changes:

- Update `cran-comments.md` to record the R versions and environments
  used for testing and the outcomes of those builds. This should all be
  `status: OK` but there might be notes that should be explained.

You now should also build the final version of the release code to be
submitted to CRAN:

``` sh
cd build_scripts
./build_and_check.sh
```

That should create the source package in the parent directory (e.g.
`safedata-1.0.6.tar.gz`).

These edits and building will obviously also need to be committed and so
there is likely to be one last round of CI runs, but this will just be
documentation and information changes and so is unlikely to reveal new
issues. Of course, if it does, you’ll have to fix them!

### Finish the release

Once the `main` branch is passing checks on all platforms, then the
candidate release is ready to be released as a version. Create a release
on the [Github release
page](https://github.com/ImperialCollegeLondon/safedata/releases) to tag
the specific commit to the `main` branch that was just used to build the
final package version.

### Release to CRAN

You can then submit the built version of the source package that was
created during the release process at:
()\[<https://cran.r-project.org/submit.html>\]

You should take the up-to-date contents of `cran-comments.md` and copy
that in the comments section of the submission form. The CRAN
maintainers expect submitted packages to be functional and fully checked
and these notes will help them see that the package has been properly
checked.
