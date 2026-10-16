# Index file memory cache

The `safedata_env` environment within the package namespace is used to
cache a copy of the dataset index, gazetteer and location aliases rather
than needing to repeatedly read these from file. This environment is
accessed by internal functions and is not intended for the end user.

## Usage

``` r
safedata_env
```
