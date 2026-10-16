# Message function that can be globally muted

Prints a message if `option("safedata.verbose")` is set to TRUE. Note
that individual expressions can be muted using
[`suppressMessages()`](https://rdrr.io/r/base/message.html) but this
mutes them globally.

## Usage

``` r
verbose_message(str, ...)
```
