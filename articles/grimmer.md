# GRIMMER basics

Granularity-related inconsistency of means mapped to error repeats, or
GRIMMER, tests whether a reported mean and standard deviation (SD) of
integer data are possible given the reported sample size (Anaya 2016;
Allard 2018). It extends GRIM (Brown and Heathers 2017): the mean must
pass GRIM, and the SD must be achievable by a sample of integers whose
sum is one of those that GRIM allows for the mean. scrutiny implements
Allard’s Analytic-GRIMMER algorithm.

The GRIMMER functions work like their GRIM counterparts, so this
vignette follows
[`vignette("grim")`](https://lhdjung.github.io/scrutiny/articles/grim.md)
and is shorter where the two tests do not differ. It covers the basics;
[`vignette("grimmer-in-depth")`](https://lhdjung.github.io/scrutiny/articles/grimmer-in-depth.md)
goes further.

### Basic GRIMMER testing

#### Single cases: `grimmer()`

To test whether a mean of 7.3 and an SD of 2.51 are consistent with a
sample size of 12:

``` r

grimmer(x = 7.3, sd = 2.51, n = 12, digits_x = 1, digits_sd = 2, items_averaged = 1, data_type = "integer")
#> [1] FALSE
```

`digits_x` and `digits_sd` are the numbers of decimal places of the
reported mean and SD. As in
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md), they
must be given because numeric values lose trailing zeros. So must
`items_averaged` and `data_type`; see
[`vignette("grim")`](https://lhdjung.github.io/scrutiny/articles/grim.md).

The SD is taken to be a sample SD, which divides by $`n - 1`$
(`sd_meaning = "sd_sample"`). That is what nearly all software reports
by default: R, SPSS, Stata, SAS, Excel, and pandas. NumPy’s `np.std()`
is the main exception and reports the population SD, which divides by
$`n`$. If the data may have been analyzed with it, set
`sd_meaning = "sd_pop"`; see
[`vignette("grimmer-in-depth")`](https://lhdjung.github.io/scrutiny/articles/grimmer-in-depth.md).

#### Many cases: `grimmer_map()`

Enter the reported values into a data frame and call
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
on it:

``` r

flying_pigs1 <- tibble::tribble(
  ~x   , ~sd  , ~n ,
   8.9 , 2.81 , 25 ,
   2.6 , 2.05 , 25 ,
   7.2 , 2.89 , 25 ,
   3.6 , 3.11 , 25 ,
   9.2 , 7.13 , 25 ,
  10.4 , 2.53 , 25 ,
   7.3 , 3.14 , 25
)

grimmer_map(flying_pigs1, digits_x = 1, digits_sd = 2, items_averaged = 1, data_type = "integer")
#> # A tibble: 7 × 9
#> # GRIMMER: default settings
#>       x    sd     n items_averaged data_type digits_x digits_sd consistency
#>   <dbl> <dbl> <int>          <int> <chr>        <int>     <int> <lgl>      
#> 1   8.9  2.81    25              1 integer          1         2 TRUE       
#> 2   2.6  2.05    25              1 integer          1         2 FALSE      
#> 3   7.2  2.89    25              1 integer          1         2 TRUE       
#> 4   3.6  3.11    25              1 integer          1         2 TRUE       
#> 5   9.2  7.13    25              1 integer          1         2 TRUE       
#> 6  10.4  2.53    25              1 integer          1         2 TRUE       
#> 7   7.3  3.14    25              1 integer          1         2 TRUE       
#> # ℹ 1 more variable: reason <chr>
```

`consistency` is the test result, and `reason` states which part of the
test an inconsistent value set failed. A value set passes GRIMMER if it
passes GRIM and the three tests of Allard (2018):

1.  The reconstructed sum of squared observations must be an integer,
    and one that integers with the reconstructed sum can reach. scrutiny
    adds this second part to Allard’s test: for example, no 11 integers
    with a sum of 40 have a sum of squares below 148, which is that of
    seven 4s and four 3s. For fewer than five values, some larger sums
    of squares are impossible, too.
2.  The reconstructed SD must match the reported one. Test 1 implies
    this, so it never appears as a reason.
3.  The sum of squares must have the same parity (odd or even) as the
    sum.

Consistent value sets are marked `"Passed all"`.

#### Scale items

For a multi-item scale, give the number of items as an `items_averaged`
argument or column, as in
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
Taken as single items, half of these value sets from a three-item scale
are wrongly flagged:

``` r

flying_pigs2 <- tibble::tribble(
  ~x   , ~sd  , ~n ,
  5.90 , 2.19 , 40 ,
  5.71 , 1.42 , 40 ,
  3.50 , 1.81 , 40 ,
  3.82 , 2.43 , 40 ,
  4.61 , 1.92 , 40 ,
  5.24 , 2.51 , 40
)

flying_pigs2 |>
  grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
#> # A tibble: 6 × 9
#> # GRIMMER: default settings
#>       x    sd     n items_averaged data_type digits_x digits_sd consistency
#>   <dbl> <dbl> <int>          <int> <chr>        <int>     <int> <lgl>      
#> 1  5.9   2.19    40              1 integer          2         2 TRUE       
#> 2  5.71  1.42    40              1 integer          2         2 FALSE      
#> 3  3.5   1.81    40              1 integer          2         2 TRUE       
#> 4  3.82  2.43    40              1 integer          2         2 TRUE       
#> 5  4.61  1.92    40              1 integer          2         2 FALSE      
#> 6  5.24  2.51    40              1 integer          2         2 FALSE      
#> # ℹ 1 more variable: reason <chr>

flying_pigs2 |>
  grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 3, data_type = "integer")
#> # A tibble: 6 × 9
#> # GRIMMER: default settings
#>       x    sd     n items_averaged data_type digits_x digits_sd consistency
#>   <dbl> <dbl> <int>          <int> <chr>        <int>     <int> <lgl>      
#> 1  5.9   2.19    40              3 integer          2         2 TRUE       
#> 2  5.71  1.42    40              3 integer          2         2 TRUE       
#> 3  3.5   1.81    40              3 integer          2         2 TRUE       
#> 4  3.82  2.43    40              3 integer          2         2 TRUE       
#> 5  4.61  1.92    40              3 integer          2         2 TRUE       
#> 6  5.24  2.51    40              3 integer          2         2 TRUE       
#> # ℹ 1 more variable: reason <chr>
```

### Explaining consistency results

[`explain_print()`](https://lhdjung.github.io/scrutiny/reference/explain_print.md)
explains the results of
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
step by step, as for GRIM. Here is the inconsistent value set from
above:

``` r

flying_pigs1 |>
  grimmer_map(digits_x = 1, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
  dplyr::filter(!consistency) |>
  explain_print()
#> GRIMMER test: x = 2.6, sd = 2.05, n = 25, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 2.6, with 1 decimal place. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 2.55 to 2.65.
#> 
#> The mean of 25 integers is their sum divided by 25. So the sum must be from
#> 2.55 * 25 = 63.75 to 2.65 * 25 = 66.25.
#> 
#> A sum of integers is itself an integer, and the integers from 64 to 66 are in
#> this range. For example, 65 / 25 = 2.6, which rounds to 2.6.
#> 
#> The SD was reported as 2.05, with 2 decimal places. Before rounding, it could
#> have been anything from 2.045 to 2.055.
#> 
#> Together, the sum and the SD determine the sum of squares, which is what you
#> get by squaring each of the 25 values and adding them up. It is
#> (25 - 1) * SD^2 + sum^2 / 25. The values are integers, so their squares are
#> integers, too, and so is the sum of squares. What's more, an integer and its
#> square are either both odd or both even. So the sum of squares must be odd if
#> the sum is odd, and even if the sum is even.
#> 
#> With a sum of 64, the sum of squares is from
#> 24 * 2.045^2 + 64^2 / 25 = 264.2086 to 24 * 2.055^2 + 64^2 / 25 = 265.1926. The
#> only integer in this range is 265, which is odd, but 64 is even.
#> 
#> With a sum of 65, the sum of squares is from
#> 24 * 2.045^2 + 65^2 / 25 = 269.3686 to 24 * 2.055^2 + 65^2 / 25 = 270.3526. The
#> only integer in this range is 270, which is even, but 65 is odd.
#> 
#> With a sum of 66, the sum of squares is from
#> 24 * 2.045^2 + 66^2 / 25 = 274.6086 to 24 * 2.055^2 + 66^2 / 25 = 275.5926. The
#> only integer in this range is 275, which is odd, but 66 is even.
#> 
#> ==> Inconsistent
```

[`explain_map()`](https://lhdjung.github.io/scrutiny/reference/explain_map.md)
adds the explanations as an `explanation` column instead, and
[`grimmer_explain()`](https://lhdjung.github.io/scrutiny/reference/grimmer_explain.md)
explains a single value set. To see the test rather than read it, draw
one value set with
[`grimmer_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grimmer_plot_line.md);
see
[`vignette("grimmer-in-depth")`](https://lhdjung.github.io/scrutiny/articles/grimmer-in-depth.md).

### Summaries with `audit()`

``` r

flying_pigs1 |>
  grimmer_map(digits_x = 1, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
  audit()
#> # A tibble: 1 × 6
#>   all_cases incon_cases emp_incon fail_grim fail_test1 fail_test3
#>       <int>       <int>     <dbl>     <int>      <int>      <int>
#> 1         7           1    0.1429         0          0          1
```

- `all_cases`, `incon_cases`, `emp_incon`: the total number of value
  sets, the number of inconsistent ones, and their ratio.
- `fail_grim`, `fail_test1`, `fail_test3`: the number of value sets that
  failed GRIM or GRIMMER’s first or third test.

### Interpreting results

An inconsistency means that an assumption of the test is false or that a
number was reported incorrectly, often for a benign reason. What to rule
out before drawing conclusions is discussed in [the section on
interpreting GRIM
results](https://trustworthyresear.ch/chapters/granularity_methods.html#sec-grim-interpreting)
of Hussey (2026). The same reasoning applies to GRIMMER.

### Going further

[`vignette("grimmer-in-depth")`](https://lhdjung.github.io/scrutiny/articles/grimmer-in-depth.md)
covers SDs that are population SDs or standard errors, bounded scales,
the means and SDs behind a test result with
[`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
and
[`grimmer_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grimmer_plot_line.md),
plots of many value sets with
[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md),
values around the reported ones with
[`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md),
and unknown group sizes with
[`grimmer_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_total_n.md).

## References

Allard, Aurélien. 2018. *Analytic-GRIMMER: A New Way of Testing the
Possibility of Standard Deviations*.
<https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/>.

Anaya, Jordan. 2016. *The GRIMMER Test: A Method for Testing the
Validity of Reported Measures of Variability*.

Brown, Nicholas J. L., and James A. J. Heathers. 2017. “The GRIM Test: A
Simple Technique Detects Numerous Anomalies in the Reporting of Results
in Psychology.” *Social Psychological and Personality Science* 8 (4):
363–69.

Hussey, Ian. 2026. *Trustworthiness Assessment and Forensic
Meta-Science*. <https://doi.org/10.5281/zenodo.23056120>.
