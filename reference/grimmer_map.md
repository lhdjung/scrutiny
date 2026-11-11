# GRIMMER-test many cases at once

`grimmer_map()` applies
[`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md)
to every row of a data frame with means, standard deviations, and sample
sizes.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics.
[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
accepts it as well.

## Usage

``` r
grimmer_map(
  data,
  digits_x,
  digits_sd,
  x = NULL,
  sd = NULL,
  n = NULL,
  ...,
  items_averaged,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
```

## Arguments

- data:

  Data frame with columns `x`, `sd`, and `n`, and optionally
  `items_averaged`, `data_type`, `digits_x`, and `digits_sd`. Any other
  columns are returned alongside the results.

- digits_x, digits_sd:

  Integer. The number of decimal places in `x` and `sd`, including
  trailing zeros. There are no defaults, since a numeric value does not
  keep them (`1.40` is `1.4`). Either a single number for the whole
  column or one number per row of `data`. If left out, they are taken
  from `digits_x` and `digits_sd` columns of `data`.

- x, sd, n:

  Optionally, the names of the columns in `data` that contain these
  values, if they differ from the defaults.

- ...:

  These dots must be empty. Every argument after them must be named in
  full.

- items_averaged, data_type:

  As in
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
  for `x` and `sd`.

- rounding, threshold, symmetric, sd_meaning:

  Passed on to
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  whose section *Defaults* explains the defaults.

## Value

A tibble with these columns:

- `x`, `sd`, `n`: the inputs.

- `items_averaged`: the number of items averaged into each score.

- `data_type`: the kind of data, as given.

- `digits_x`, `digits_sd`: the numbers of decimal places, as given.

- `consistency`: GRIMMER consistency of `x`, `sd`, `n`, and
  `items_averaged`. `NA` for a case that cannot be decided, such as one
  with a missing value.

- `reason`: `"Passed all"` if consistent, or `"Continuous data"` if the
  test does not apply; otherwise the test that failed, or why the case
  cannot be decided.

- Any other columns from `data`.

The `reason` column refers to GRIM and the GRIMMER tests of Allard
(2018):

1.  The reconstructed sum of squared observations must be a whole
    number, and one that `n` whole numbers with the reconstructed sum
    can reach: at least the sum of squares of these numbers as close
    together as possible. For `n < 5`, some larger sums of squares are
    impossible, too.

2.  The reconstructed SD must match the reported one. Test 1 implies
    this, so it is never a reason.

3.  The parity of the reconstructed sum of squared observations must
    match that of the reconstructed sum of the values.

A negative `sd` gives `"Negative SD"`.

Under `sd_meaning = "sd_sample_or_pop"`, a consistent value set gives
`"Passed all (sample SD)"` or, if it passes as a population SD only,
`"Passed all (population SD)"`. If the two conventions fail for
different reasons, the `reason` names both, and
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
counts the sample SD's, as under the default `sd_meaning`.

The tibble has the class `scrutiny_grimmer_map`, which is recognized by
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) and
[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md).

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`grimmer_map()` output returns a tibble with one row and these columns:

1.  `all_cases`: total number of value sets.

2.  `incon_cases`: number of GRIMMER-inconsistent value sets.

3.  `emp_incon`: proportion of GRIMMER-inconsistent value sets.

4.  `fail_grim`: number of value sets that fail GRIM.

5.  `fail_test1`, `fail_test3`: number of value sets that fail GRIMMER's
    first or third test. Test 2 is implied by test 1, so it never fails.

## References

Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
possibility of standard deviations.
https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/

Anaya, J. (2016). The GRIMMER test: A method for testing the validity of
reported measures of variability. *PeerJ Preprints.*
https://peerj.com/preprints/2400v1/

## See also

[`explain_map()`](https://lhdjung.github.io/scrutiny/reference/explain_map.md)
and
[`explain_print()`](https://lhdjung.github.io/scrutiny/reference/explain_print.md)
to explain each row's result in plain words.

## Examples

``` r
# Use `grimmer_map()` on data like these:
pigs5
#> # A tibble: 12 × 3
#>        x    sd     n
#>    <dbl> <dbl> <dbl>
#>  1  7.22  5.3     38
#>  2  4.74  6.55    31
#>  3  5.23  2.55    35
#>  4  2.57  2.57    30
#>  5  6.77  2.18    33
#>  6  2.68  2.59    34
#>  7  7.01  6.68    35
#>  8  7.38  3.65    32
#>  9  3.14  5.32    33
#> 10  6.89  4.18    37
#> 11  5     2.18    31
#> 12  0.24  6.43    34

# The `consistency` column shows whether the values to its left are
# GRIMMER-consistent. If they aren't, the `reason` column gives the reason:
pigs5 |>
  grimmer_map(
    digits_x = 2,
    digits_sd = 2,
    items_averaged = 1,
    data_type = "integer"
  )
#> # A tibble: 12 × 9
#> # GRIMMER: default settings
#>        x    sd     n items_averaged data_type digits_x digits_sd consistency
#>    <dbl> <dbl> <int>          <int> <chr>        <int>     <int> <lgl>      
#>  1  7.22  5.3     38              1 integer          2         2 FALSE      
#>  2  4.74  6.55    31              1 integer          2         2 TRUE       
#>  3  5.23  2.55    35              1 integer          2         2 FALSE      
#>  4  2.57  2.57    30              1 integer          2         2 TRUE       
#>  5  6.77  2.18    33              1 integer          2         2 FALSE      
#>  6  2.68  2.59    34              1 integer          2         2 TRUE       
#>  7  7.01  6.68    35              1 integer          2         2 FALSE      
#>  8  7.38  3.65    32              1 integer          2         2 TRUE       
#>  9  3.14  5.32    33              1 integer          2         2 FALSE      
#> 10  6.89  4.18    37              1 integer          2         2 TRUE       
#> 11  5     2.18    31              1 integer          2         2 TRUE       
#> 12  0.24  6.43    34              1 integer          2         2 TRUE       
#> # ℹ 1 more variable: reason <chr>

# Get summaries with `audit()`:
pigs5 |>
  grimmer_map(
    digits_x = 2,
    digits_sd = 2,
    items_averaged = 1,
    data_type = "integer"
  ) |>
  audit()
#> # A tibble: 1 × 6
#>   all_cases incon_cases emp_incon fail_grim fail_test1 fail_test3
#>       <int>       <int>     <dbl>     <int>      <int>      <int>
#> 1        12           5     0.417         4          0          1

# Explain the results in plain words:
pigs5 |>
  grimmer_map(
    digits_x = 2,
    digits_sd = 2,
    items_averaged = 1,
    data_type = "integer"
  ) |>
  explain_print(1:2)
#> GRIMMER test: x = 7.22, sd = 5.30, n = 38, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 7.22, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 7.215 to 7.225.
#> 
#> The mean of 38 integers is their sum divided by 38. So the sum must be from
#> 7.215 * 38 = 274.17 to 7.225 * 38 = 274.55.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 274 and 275. They give 274 / 38 = 7.2105 and
#> 275 / 38 = 7.2368, which round to 7.21 and 7.24, not to 7.22. Since the mean
#> alone is impossible, the SD doesn't need to be checked.
#> 
#> ==> Inconsistent
#> 
#> GRIMMER test: x = 4.74, sd = 6.55, n = 31, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 4.74, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 4.735 to 4.745.
#> 
#> The mean of 31 integers is their sum divided by 31. So the sum must be from
#> 4.735 * 31 = 146.785 to 4.745 * 31 = 147.095.
#> 
#> A sum of integers is itself an integer, and 147 is in this range:
#> 147 / 31 = 4.7419, which rounds to 4.74.
#> 
#> The SD was reported as 6.55, with 2 decimal places. Before rounding, it could
#> have been anything from 6.545 to 6.555.
#> 
#> Together, the sum and the SD determine the sum of squares, which is what you
#> get by squaring each of the 31 values and adding them up. It is
#> (31 - 1) * SD^2 + sum^2 / 31. The values are integers, so their squares are
#> integers, too, and so is the sum of squares. What's more, an integer and its
#> square are either both odd or both even. So the sum of squares must be odd if
#> the sum is odd, and even if the sum is even.
#> 
#> With a sum of 147, the sum of squares is from
#> 30 * 6.545^2 + 147^2 / 31 = 1982.1753 to 30 * 6.555^2 + 147^2 / 31 = 1986.1053.
#> The integers from 1983 to 1986 are in this range, so one of them is odd, like
#> 147.
#> 
#> ==> Consistent
#> 
```
