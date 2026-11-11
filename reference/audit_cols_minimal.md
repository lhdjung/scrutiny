# Compute minimal `audit()` summaries

Call `audit_cols_minimal()` within an
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
method for the output of a consistency test mapper such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
It returns a tibble with the three columns every such method must have:

1.  `all_cases`: the number of rows, including undecidable ones.

2.  `incon_cases`: the number of rows where `consistency` is `FALSE`. A
    case the test could not decide is `NA` there and not counted.

3.  `emp_incon`: the empirical rate of inconsistency,
    `incon_cases / all_cases`.

Add further columns as needed. See *Examples* for how to name the
method.

## Usage

``` r
audit_cols_minimal(data)
```

## Arguments

- data:

  Data frame returned by a mapper function.

## Value

A tibble with the three columns above.

## See also

[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md).

## Examples

``` r
# For a mapper `schlim_map()` that applies a test called SCHLIM and
# returns a data frame with the class `"scrutiny_schlim_map"`:
audit.scrutiny_schlim_map <- function(data, ...) {
  audit_cols_minimal(data)
}
```
