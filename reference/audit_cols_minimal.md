# Compute minimal `audit()` summaries

Call `audit_cols_minimal()` within your
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
methods for the output of consistency test mapper functions such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
It will create a tibble with the three minimal, required columns:

1.  `incons_cases` counts the inconsistent cases, i.e., the number of
    rows in the mapper's output where `"consistency"` is `FALSE`.

2.  `all_cases` is the total number of rows in the mapper's output.

3.  `incons_rate` is the ratio of `incons_cases` to `all_cases`.

You can still add other columns to this tibble. Either way, make sure to
name your method correctly. See examples.

## Usage

``` r
audit_cols_minimal(data, name_test)
```

## Arguments

- data:

  Data frame returned by a mapper function, such as
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).

- name_test:

  String (length 1). Short, plain-text name of the consistency test,
  such as `"GRIM"`. Only needed for a potential alert.

## Value

A tibble (data frame) with the columns listed above.

## See also

For context, see
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md).
In case you don't call `audit_cols_minimal()`, you should call
[`check_audit_special()`](https://lhdjung.github.io/scrutiny/reference/check_audit_special.md).

## Examples

``` r
# For a mapper function called `schlim_map()`
# that applies a test called SCHLIM and returns
# a data frame with the `"scr_schlim_map"` class:
audit.scr_schlim_map <- function(data) {
  audit_cols_minimal(data, name_test = "SCHLIM")
}

# If you like, add other summary columns
# with `dplyr::mutate()` or similar.
```
