# Summarize scrutiny objects

`audit()` summarizes the output of scrutiny functions that test data
frames, such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
It is an S3 generic with methods for the functions listed below. The
columns of each summary are described in the documentation of the
respective function.

On the output of a sequence or total-n mapper, such as
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md),
it returns one row per case; see
[audit-special](https://lhdjung.github.io/scrutiny/reference/audit-special.md).

## Usage

``` r
audit(data, ..., .by = NULL)
```

## Arguments

- data:

  A data frame returned by one of the functions below.

- ...:

  These dots must be empty.

- .by:

  Optionally, columns of `data` to summarize by, selected as in
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
  such as `.by = study`. Each group is summarized on its own, and its
  rows start with the grouping columns. Groups appear in the order of
  their first row. Use this rather than
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
  which removes the class that `audit()` dispatches on.

## Value

A tibble with summary statistics.

## Run before `audit()`

|  |  |
|----|----|
| **Function** | **Class** |
| [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md) | `"scrutiny_grim_map"` |
| [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md) | `"scrutiny_grimmer_map"` |
| [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md) | `"scrutiny_debit_map"` |
| [`percentage_of_map()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map.md) | `"scrutiny_percentage_of_map"` |
| [`add_up_to_map()`](https://lhdjung.github.io/scrutiny/reference/add_up_to_map.md) | `"scrutiny_add_up_to_map"` |
| `*_map_seq()` functions | `"scrutiny_map_seq"` |
| `*_map_total_n()` functions | `"scrutiny_map_total_n"` |
| [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md) | `"scrutiny_dup_count"` |
| [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md) | `"scrutiny_dup_count_colpair"` |
| [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md) | `"scrutiny_dup_tally"` |
| `audit()` on seq output | `"scrutiny_audit_seq"` |
| `audit()` on total-n output | `"scrutiny_audit_total_n"` |

## Examples

``` r
pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  audit()
#> # A tibble: 1 × 6
#>   all_cases testable_cases incon_cases emp_incon exp_incon ratio
#>       <int>          <int>       <int>     <dbl>     <dbl> <dbl>
#> 1        12             12           8     0.667     0.718 0.929

pigs4 |>
  duplicate_count() |>
  audit()
#> # A tibble: 2 × 8
#>   term         mean    sd median   min   max na_count na_rate
#>   <chr>       <dbl> <dbl>  <int> <int> <int>    <int>   <dbl>
#> 1 frequency    1.36 0.674      1     1     3        0       0
#> 2 locations_n  1.27 0.647      1     1     3        0       0

# Summaries by group:
pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  dplyr::mutate(study = rep(c("A", "B", "C"), each = 4)) |>
  audit(.by = study)
#> # A tibble: 3 × 7
#>   study all_cases testable_cases incon_cases emp_incon exp_incon ratio
#>   <chr>     <int>          <int>       <int>     <dbl>     <dbl> <dbl>
#> 1 A             4              4           3      0.75     0.705 1.06 
#> 2 B             4              4           2      0.5      0.725 0.690
#> 3 C             4              4           3      0.75     0.722 1.04 
```
