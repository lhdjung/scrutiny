# Summarize scrutiny objects

`audit()` summarizes the results of scrutiny functions like
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
that perform tests on data frames.

See below for a record of such functions. Go to the documentation of any
of them to learn about its `audit()` method.

## Usage

``` r
audit(data)
```

## Arguments

- data:

  A data frame that inherits one of the classes named below.

## Value

A tibble (data frame) with test summary statistics.

## Details

`audit()` is an S3 generic. It looks up the (invisible) scrutiny class
of a tibble returned by any function named below. You don't need to deal
with the classes directly. Behind the scenes, they mediate between these
functions and their associated summary statistics.

## Run before `audit()`

|                                                                                                        |                           |
|--------------------------------------------------------------------------------------------------------|---------------------------|
| **Function**                                                                                           | **Class**                 |
| [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)                               | `"scr_grim_map"`          |
| [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)                         | `"scr_grimmer_map"`       |
| [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)                             | `"scr_debit_map"`         |
| [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)                 | `"scr_dup_count"`         |
| [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md) | `"scr_dup_count_colpair"` |
| [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)                 | `"scr_dup_tally"`         |
| [`duplicate_detect()`](https://lhdjung.github.io/scrutiny/reference/duplicate_detect.md)               | `"scr_dup_detect"`        |
| [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)                         | `"scr_audit_seq"`         |
| [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)                     | `"scr_audit_total_n"`     |

## Examples

``` r
# For basic GRIM-testing:
pigs1 %>%
  grim_map() %>%
  audit()
#> # A tibble: 1 × 7
#>   incons_cases all_cases incons_rate mean_grim_prob incons_to_prob
#>          <int>     <int>       <dbl>          <dbl>          <dbl>
#> 1            8        12       0.667          0.724          0.921
#> # ℹ 2 more variables: testable_cases <int>, testable_rate <dbl>

# For duplicate detection:
pigs4 %>%
  duplicate_count() %>%
  audit()
#> # A tibble: 2 × 8
#>   term         mean    sd median   min   max na_count na_rate
#>   <chr>       <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>   <dbl>
#> 1 frequency    1.36 0.674      1     1     3        0       0
#> 2 locations_n  1.27 0.647      1     1     3        0       0
```
