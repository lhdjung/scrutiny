# Apply DEBIT to many cases

Call `debit_map()` to use DEBIT on multiple combinations of mean, sample
standard deviation, and sample size of binary distributions. Mapping
function for
[`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md).

For summary statistics, call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
the results.

## Usage

``` r
debit_map(
  data,
  x = NULL,
  sd = NULL,
  n = NULL,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  show_rec = TRUE,
  extra = Inf
)
```

## Arguments

- data:

  Data frame.

- x, sd, n:

  Optionally, specify these arguments as column names in `data`.

- rounding, threshold, symmetric:

  Arguments passed on to
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
  with the same defaults.

- show_rec:

  If set to `FALSE`, the resulting tibble only includes the columns `x`,
  `sd`, `n`, and `consistency`. Default is `TRUE`.

- extra:

  Not currently used.

## Value

A tibble with (at least) these columns –

- `x`, `sd`, `n`: the inputs.

- `consistency`: DEBIT consistency of `x`, `sd`, and `n`.

  By default, the tibble also includes the rounding method, boundary
  values, and information about the boundary values being inclusive or
  not. The tibble has the `scr_debit_map` class, which is recognized by
  the [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  generic.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

There is an S3 method for the
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
generic, so you can call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
following `debit_map()`. It returns a tibble with these columns —

1.  `incons_cases`: the number of DEBIT-inconsistent cases.

2.  `all_cases`: the total number of cases.

3.  `incons_rate`: the rate of inconsistent cases.

4.  `mean_x`: the mean `x` (mean) value.

5.  `mean_sd`: the mean `sd` value.

6.  `distinct_n`: the number of distinct `n` values.

## References

Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A Simple
Consistency Test For Binary Data. https://osf.io/5vb3u/.

## Examples

``` r
# Call `debit_map()` on binary summary
# data such as these:
pigs3
#> # A tibble: 7 × 3
#>   x     sd        n
#>   <chr> <chr> <dbl>
#> 1 0.53  0.50   1683
#> 2 0.44  0.50   1683
#> 3 0.77  0.42   1683
#> 4 0.19  0.35   1683
#> 5 0.34  0.47   1683
#> 6 0.93  0.25   1683
#> 7 0.12  0.33   1683

# The `consistency` column shows
# whether the values to its left
# are DEBIT-consistent:
pigs3 %>%
  debit_map()
#> # A tibble: 7 × 11
#>   x     sd        n consistency rounding   sd_lower sd_incl_lower sd_upper
#>   <chr> <chr> <int> <lgl>       <chr>         <dbl> <lgl>            <dbl>
#> 1 0.53  0.50   1683 TRUE        up_or_down    0.495 TRUE             0.505
#> 2 0.44  0.50   1683 TRUE        up_or_down    0.495 TRUE             0.505
#> 3 0.77  0.42   1683 TRUE        up_or_down    0.415 TRUE             0.425
#> 4 0.19  0.35   1683 FALSE       up_or_down    0.345 TRUE             0.355
#> 5 0.34  0.47   1683 TRUE        up_or_down    0.465 TRUE             0.475
#> 6 0.93  0.25   1683 TRUE        up_or_down    0.245 TRUE             0.255
#> 7 0.12  0.33   1683 TRUE        up_or_down    0.325 TRUE             0.335
#> # ℹ 3 more variables: sd_incl_upper <lgl>, x_lower <dbl>, x_upper <dbl>

# Get test summaries with `audit()`:
pigs3 %>%
  debit_map() %>%
  audit()
#> # A tibble: 1 × 6
#>   incons_cases all_cases incons_rate mean_x mean_sd distinct_n
#>          <int>     <int>       <dbl>  <dbl>   <dbl>      <int>
#> 1            1         7       0.143  0.474   0.403          1
```
