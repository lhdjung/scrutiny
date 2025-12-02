# Documentation template for [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

`write_doc_audit()` creates a roxygen2 block section to be inserted into
the documentation of a mapper function such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
or
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md):
functions for which there are, or should be,
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
methods. The section informs users about the ways in which
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
summarizes the results of the respective mapper function.

Copy the output from your console and paste it into the roxygen2 block
of your `*_map()` function. To preserve the numbered list structure when
indenting roxygen2 comments with `Ctrl`+`Shift`+`/`, leave empty lines
between the pasted output and the rest of the block.

## Usage

``` r
write_doc_audit(sample_output, name_test)
```

## Arguments

- sample_output:

  Data frame. Result of a call to
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
  a data frame that resulted from a call to the mapper function for
  which you wrote the
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  method, such as `audit(grim_map(pigs1))` or `audit(debit_map(pigs3))`.

- name_test:

  String (length 1). Name of the consistency test which the mapper
  function applies, such as `"GRIM"` or `"DEBIT"`.

## Value

A string vector formatted by
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

## Examples

``` r
# Start by running `audit()`:
out_grim  <- audit(grim_map(pigs1))
out_debit <- audit(debit_map(pigs3))

out_grim
#> # A tibble: 1 × 7
#>   incons_cases all_cases incons_rate mean_grim_prob incons_to_prob
#>          <int>     <int>       <dbl>          <dbl>          <dbl>
#> 1            8        12       0.667          0.724          0.921
#> # ℹ 2 more variables: testable_cases <int>, testable_rate <dbl>
out_debit
#> # A tibble: 1 × 6
#>   incons_cases all_cases incons_rate mean_x mean_sd distinct_n
#>          <int>     <int>       <dbl>  <dbl>   <dbl>      <int>
#> 1            1         7       0.143  0.474   0.403          1

# Documenting the `audit()` method for `grim_map()`:
write_doc_audit(sample_output = out_grim, name_test = "GRIM")
#> #' @section Summaries with `audit()`: There is an S3 method for `audit()`, so 
#> #'   you can call `audit()` following `grim_map()` to get a summary of 
#> #'   `grim_map()`'s results. It is a tibble with a single row and these 
#> #'   columns -- 
#> #' 
#> #' 1. `incons_cases`: number of GRIM-inconsistent value sets.
#> #' 2. `all_cases`: total number of value sets.
#> #' 3. `incons_rate`: proportion of GRIM-inconsistent value sets.
#> #' 4. `mean_grim_prob`: 
#> #' 5. `incons_to_prob`: 
#> #' 6. `testable_cases`: 
#> #' 7. `testable_rate`: 

# Documenting the `audit()` method for `debit_map()`:
write_doc_audit(sample_output = out_debit, name_test = "DEBIT")
#> #' @section Summaries with `audit()`: There is an S3 method for `audit()`, so 
#> #'   you can call `audit()` following `debit_map()` to get a summary of 
#> #'   `debit_map()`'s results. It is a tibble with a single row and these 
#> #'   columns -- 
#> #' 
#> #' 1. `incons_cases`: number of DEBIT-inconsistent value sets.
#> #' 2. `all_cases`: total number of value sets.
#> #' 3. `incons_rate`: proportion of DEBIT-inconsistent value sets.
#> #' 4. `mean_x`: 
#> #' 5. `mean_sd`: 
#> #' 6. `distinct_n`: 
```
