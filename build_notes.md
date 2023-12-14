# Developing and releasing `safedata`

This repository and package uses a number of systems that you need to be aware
of if you plan to work with the codebase:

* The repository uses the git flow approach to maintaining `develop` and `main`
  branches and uses continuous integration, with changes accepted into `develop` or
  `main` only through pull requests.
* The repository uses GitHub Actions to carry out out checks on pushes and pull
  requests.
* Documentation for the package is maintained using `roxygen` and `pkgdown`.
* The code is checked using linting for consistent style.

These notes provide a brief walk through for these systems and some package specific
requirements for releases.

## Code development

Any new feature or bug fix should start with a new [GitHub
issue](https://github.com/ImperialCollegeLondon/safedata/issues) and then the creation
of a new branch to implement the new code. When that branch is complete and passing
checks then a pull request onto the `develop` branch should be made.

A new release start from the `develop` branch, following merged pull requests for
features and bug fixes that are going to be included in the new release. A suitably
named `release` branch should be made to check and make final changes. That `release`
branch is then merged into `main` and tagged as the new release, and also back into
`develop` to bring back last minute fixes. The `main` branch should only ever
see merges in from a `release` branch - you must not work on it directly.

The package uses ([semantic version numbering](https://semver.org/)) and code on the
development branch should use the prerelease token `9000`. This is explained in more
detail below in the description of the release cycle.

You should install the R `devtools` package as well as the `safedata` package
dependencies.

## GitHub Actions

When commits are pushed to the Github origin then the package is automatically
built and checked using GitHub Actions:

[https://github.com/ImperialCollegeLondon/safedata/actions](https://github.com/ImperialCollegeLondon/safedata/actions)

Checking happens on pushes to all branches, so day to day commits to feature branches
will be built as well as pull requests onto `develop` and merges onto `main` from
`release` branches. If you have made changes that you do not want to be built and
checked then you can include `[ci skip]` in the commit message, but the idea is that all
changes should be checked so this is typically only used for minor documentation changes
and the like.

The currently configured actions do the following:

* On `push` or `pull_requests` to `feature`, `develop` or `main` branches, the actions
  run:
  * Quality assurance checks by running `pre-commit` using the configured R
    `precommit` steps.
  * Package build checks by running `R CMD CHECK --as-cran`
  * Documentation checks using `pkgdown::build_site()`.
* On `pull_requests` to main, a second action will run `pkgdown::deploy_to_branch()`
  to publish the new documentation for the released version.

## The `pre-commit` setup

`pre-commit` is a Python program that runs a set of hooks when any changes are committed
to the repository. These hooks are used to check that the code is free of any common
issues _before_ it is committed - if the checks fail, the commit will be aborted and you
will need to fix the issues and re-commit your changes.

You will need to:

1. Install [`pre-commit`](https://pre-commit.com/#install)
2. Install the `pre-commit` configuration for the package from within your clone of the
   `safedata` repositrory by using: `pre-commit install`

At present, the automatic checking includes:

* Code styling
* Regeneration of the R documentation (`man`) files to ensure the source code and
  documentation are synchronised (see below).
* Checking that the DESCRIPTION file structure is correct and contains all package
  dependencies used in the code.
* Automatic linting of the R source files.
* Basic checks that the R code is parsable and contains no `browser()` or `debug()`
  statements.
* Checks for common file issues: sorting, missing line endings, oversized files and the
  inclusion of 'artifact' files such as `.Rhistory` files.

Note that _some_ of these steps may make changes to your files to fix issues, such as to
update `man` files and apply common styling. When this happens, the `pre-commit` step
will fail but you can simply add the changed files to the commit and re-commit to add
the automatic fixes.

## Package documentation

With the `roxygen2` package , all of the package documentation content is located in
source files. These are then processed to automatically generate the standard R
documentation files (the `man/*.Rd` files).

### The `man` pages

The `man` directory contains all of the package documentation as `.Rd` format files, but
these files are built automatically by the `roxygen2` package from docstrings in the `R`
source files.  When these source files are changed, that documentation content needs to
be rebuilt to make sure all of the content is up to date. This step is built into the
`pre-commit` checking, so commits should automatically include any updates to the `man`
pages.

If you do want to rebuild `man` pages manually - to check what changes look like before
committing - the command is:

```sh
Rscript -e "devtools::document()"
```

### Vignettes

The source content for vignettes are `Rmarkdown` files in the `vignettes` directory, but
these need to be built into HTML files using the `knitr` package and installed in the
`inst/doc` directory. The following tool can be used to rebuild the vignettes:

```sh
Rscript -e "devtools::build_vignettes()"
```

However, the `R CMD BUILD` process, which is used to generate the source R package for
release also builds the vignettes and this must pass without error on GitHub Action
checking.

### Website

In addition to using `roxygen2` and `knitr` to maintain the package documentation and
vignettes, `safedata` also uses `pkgdown` to create a documentation website. This
basically builds a complete website and index around the `Rd` files as an API reference
and the vignettes as explanatory content.

The configuration for this is in the `_pkgdown.yml` file. This file typically only needs
updating when new functions are added, as they need to be added into the reference
structure. The website will contain an HTML version of the Rd files and vignettes but
also extra content: for example `README.md` is used to create `index.html` and other
markdown files can be used to provide other content.

Rebuilding the website uses two commands:

```sh
Rscript -e " pkgdown::clean_site()"
Rscript -e " pkgdown::build_site()"
```

This removes old files and recreates the website in the `docs` directory (_not_ `doc`!).
This directory is then automatically deployed by GitHub Actions to the Github Pages
package website at:

[<https://imperialcollegelondon.github.io/safedata/](https://imperialcollegelondon.github.io/safedata/)>

The website files themselves are _not_ part of the continuous integration of the package
and having to build and then commit changes within `docs` is untidy. The `safedata`
package therefore includes `docs` in the `.gitignore` file: you can have a local copy of
the website but it is not managed by git.

Instead, a GitHub Action has been configured to run `pkgdown` - when a new release has
passed checks, the action can be triggered in order to update the `gh-pages` branch
using the most recent code.

For local use, the package and website build steps have been bundled together in
`build_scripts/build_docs.sh`.

## Package test suite

The package contains a test suite used for testing that functions behave as expected.
Currently, this largely checks that network failures are handled gracefully. Those have
to be passing before the package can be released. The tests are automatically run by `R
CMD CHECK` during Github Actions but can also be run locally using:

```sh
Rscript -e "devtools::test()"
```

## Web mocking

The `safedata` package uses the internet extensively for downloading package data and
metadata. This includes code used in function examples, vignettes and in the test suite.
The private function `safedata:::mock_api()` is used in all of these situations to turn
off connections to the actual internet and instead serve up content from the
`inst/api_data` folder. This ensures that testing and documentation building is not
reliant on internet connections. If you add new code to the project that requests
content from the internet, you will need to extend the set of mocked URLS in the
`mock_api_urls` list in `R/api_mockr.R` and include links to the expected content from
those links.

At some point, this needs to be made optional, to allow package build to also be tested
against live content.

## Linting

The linting process inspects the R code files in the package to check they have a
consistent coding and syntax style. Linting is run automatically during `pre-commit`
checks but running the command below from the package root will also generate a linting
report.

```sh
Rscript -e "lintr::lint_package()"
```

The `.lintr` file in the package root is used to configure the linting and set any
exclusions. Code lines can be excluded using the `# nolint` tag but this doesn't work
well for comments and docstrings as code formatters aggresively wrap lines on save. For
these lines `.lintr` has to be updated to exclude linting on specific line numbers.

Currently, the `.lintr` configuration:

* sets a very lax limit on cyclomatic complexity,
* permits commented out code - there are a couple of temporarily retired tests,
* updates the indenting depth to 4 and not the default 2 spaces, and
* turns off object usage checking, this needs to be renabled at some point.

## Release cycle

These are the steps needed to release a new version of `safedata`.  Using pull requests
into `develop` and GitHub Actions should ensure that `develop` is always building
correctly but checking should be repeated during the release process to ensure that
everything is up to date.

### Configure `git`

It is easier if `git` is configured to push new tags along with commits. This
essentially just means that new releases can be sent with a single commit, which is
simpler and saves GitHub Actions from building both the the code commit and then the
tagged version. This only needs to be set once.

```sh
set git config --global push.followTags true
```

### Create the new release branch

The following creates a new candidate branch containing the current `develop` code. You
need to specify the upcoming release version number, so for example to release version
`1.0.6`:

```sh
git flow release start 1.0.6
```

This will create the `release/1.0.6` branch and check it out.

You should now immediately update the `DESCRIPTION` file to match that version
number. In this example, that should mean changing the previous development
version number (`-9000` is used to indicate code in development between versions
):

```txt
Version: 1.0.5-9000
```

to

```txt
Version: 1.0.6
```

You can then commit that change:

```sh
git commit -m "Update version number" DESCRIPTION
```

At the moment, the `release` branch is only local. The release branch and code needs to
be pushed to Github to be picked up GitHub Actions. There is a specific `git flow`
command to do this:

```sh
git flow release publish 1.0.6
```

This sends the release branch up to be checked. In addition, there is now a release
branch on origin so any other last minutes fixes and commits can be pushed in order to
check those.

### Double check the code

All of the checking steps are compiled into a single script in
`build_scripts/build_and_check.sh`. The content is:

```sh
#!/bin/bash
# Script to build and check safedata

# 0) Show R version being used
R --version

printf "\n------------\n\n"

# 1) Move down into the source directory and update the documents using Roxygen and pkgdown
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

1. The built package and check output are saved in the parent directory of the
   repository, not in the repository itself.
2. The version built is the one in the _currently checked out branch_!

The key file to look at is `safedata.Rcheck/00check.log`. This contains a long
list of checks applied to the code. Look out for `NOTE`, `WARNING` and `ERROR`
and resolve these issues before moving on. If you are checking in the `develop`
branch then you will see a note saying `Version contains large components` -
that is about to be fixed.

### Checking on different platforms

The GitHub Action build process should now be underway for the `release` branch. Git
Actions is configured (see `.github/workflows/check-standard.yaml`) to build the package
under R stable on Ubuntu, Mac and Windows and R devel on Ubuntu.

Although Windows is included in the GitHub Actions testing environment, the R Project
also maintains a Windows test environment that can be used. This needs a built copy of
the `release` branch, so run `build_scripts/build_and_check.sh` again. This should
create a newly built package with the new version number (e.g. `safedata_1.0.6.tar.gz`).
If everything checked out ok before creating the release, this is really just updating
the version name.

You then need to upload that file to `win-builder`. The python script
`build_scripts/upload_to_win-builder.py`  will do this for you - it is simply automating
the process of using FTP to upload the current version for checking under both R stable
and R devel. Note that `win-builder` communicates by email with the package maintainer
(whoever has the `cre` flag in the `authors` section of the `DESCRIPTION` file).

```sh
python build_scripts/upload_to_win-builder.py
```

### Wait

Ideally what happens now is that the build and check process on GitHub Actions and
`win-builder` all pass. You **must** wait for these checks to complete!

Obviously, if any errors or warnings crop up in the checking process, those should be
fixed in the `release` branch. The changes should be committed and pushed to start a new
round of GitHub Actions checking and you will need to rebuild and resubmit to
`win-builder`

### Final edits

There are some final edits to check you have made:

* Update `NEWS` to document the changes since the previous version
* Update `cran-comments.md` to record the R versions and environments used for testing
  and the outcomes of those builds. This should all be `status: OK` but there might be
  notes that should be explained.

You now should also build the final version of the release code to be submitted to CRAN:

```sh
cd build_scripts
./build_and_check.sh
```

That should create the source package in the parent directory (e.g.
`safedata-1.0.6.tar.gz`).

These edits and building will obviously also need to be committed and so there is likely
to be one last round of CI runs, but this will just be documentation and information
changes and so is unlikely to reveal new issues. Of course, if it does, you'll have to
fix them!

### Finish the release

Once the `release` branch is passing checks on all platforms, then the candidate release
is ready to be released as a version.  Again using `1.0.6` as the example version
number, the command is:

```sh
git flow release finish 1.0.6
```

You will be asked for some commit messages and a new tag comment, which will simply be
the version number. You should then be on the `develop` branch. You now need to checkout
the `main` branch which should now have all the commits since the last release and a
new tag with the version number. You can now push this to create the release - if you've
set the config described above then a single push will create the commit and tag.

```sh
git checkout main
git push
```

This will set off another round of GitHub Actions checking - you should see the tagged
version being built and checked. This should all go cleanly!

You should now **immediately** get off the `main` branch and back onto `develop`,
before you accidentally change the files or commit to it, You should also
**immediately** update the version number in `DESCRIPTION`, adding `-9000` to show that
this is now the development version from the new release. This is a trivial change, so
we can use `[ci skip]` to avoid triggering Github Actions.

```sh
git checkout develop
# Edit DESCRIPTION to e.g. version 1.0.6-9000
git commit -m "Bump develop version [ci skip]" DESCRIPTION
git push
```

### Release to CRAN

You can then submit the built version of the source package that was created during the
release process at: ()[https://cran.r-project.org/submit.html]

You  should  take the up-to-date contents of `cran-comments.md` and  copy that in the
comments section of the submission form. The CRAN maintainers expect submitted packages
to be functional and fully checked and these notes will help them see that the package
has been properly checked.
