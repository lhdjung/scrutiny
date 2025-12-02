# Unnest a test result column

Within a consistency test mapper function, it may become necessary to
unpack a column resulting from a basic `*_scalar()` testing function.
That will be the case if a `show_*` argument of the mapper function like
`show_rec` in
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
is `TRUE`, and the `*_scalar()` function returns a list of values, not
just a single value.

At the point where such as list is stored in a data frame column (most
likely `"consistency"`), call `unnest_consistency_cols()` to unnest the
results into multiple columns.

## Usage

``` r
unnest_consistency_cols(results, col_names, index = FALSE, col = "consistency")
```

## Arguments

- results:

  Data frame containing a list-column by the name passed to `col`.

- col_names:

  String vector of new names for the unnested columns. It should start
  with the same string that was given for `col`.

- index:

  Logical. Should the list-column be indexed into? Default is `FALSE`.

- col:

  String (length 1). Name of the list-column within `results` to operate
  on. Default is `"consistency"`.

## Value

Data frame. The column names are determined by `col_names`.

## Details

This function is a custom workaround in place of
[`tidyr::unnest_wider()`](https://tidyr.tidyverse.org/reference/unnest_wider.html),
mirroring some of the latter's functionality. It was created because
`unnest_wider()` can be too slow for use as a helper function.

## See also

[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md),
for context.
