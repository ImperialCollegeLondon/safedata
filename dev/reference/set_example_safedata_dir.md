# Functions to use an example data directory for package examples

The documentation of `safedata` includes code examples that require a
real data directory but package code must not write within the package
structure. The `set_example_safedata_dir` function is used in code
examples to create a safedata directory using local resources in a
temporary directory. This example directory should only be created once
per session.

## Usage

``` r
set_example_safedata_dir(on = TRUE)
```

## Arguments

- on:

  Turn the use of the example data directory on or off.

## Value

The set_example_safedata_dir function returns the path of the example
directory invisibly (on = TRUE) or the path of any restored user data
directory (on =FALSE).

## Details

The argument `on=FALSE` can be used to restore any existing data
directory set by the user.

## See also

[`set_safedata_dir`](https://imperialcollegelondon.github.io/safedata/dev/reference/set_safedata_dir.md)
