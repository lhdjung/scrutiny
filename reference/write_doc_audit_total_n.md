# Documentation template for [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)

`write_doc_audit_total_n()` creates a roxygen2 block section to be
inserted into the documentation of functions created with
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md).
The section informs users about the ways in which
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
summarizes the results of the manufactured `*_map_total_n()` function.

Copy the output from your console and paste it into the roxygen2 block
of your `*_map_total_n()` function. To preserve the bullet-point
structure when indenting roxygen2 comments with `Ctrl`+`Shift`+`/`,
leave empty lines between the pasted output and the rest of the block.

## Usage

``` r
write_doc_audit_total_n(key_args, name_test)
```

## Arguments

- key_args:

  String vector with the names of the key columns that are tested for
  consistency by the `*_map_seq()` function. (These are the original
  variable names, without `"1"` and `"2"` suffixes.) The values need to
  have the same order as in that function's output.

- name_test:

  String (length 1). Name of the consistency test which the
  `*_map_seq()` function applies, such as `"GRIM"`.

## Value

A string vector formatted by
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

## See also

The sister function
[`write_doc_audit_seq()`](https://lhdjung.github.io/scrutiny/reference/write_doc_audit_seq.md)
and, for context,
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md).

## Examples

``` r
# For GRIM and `grim_map_total_n()`:
write_doc_audit_total_n(key_args = c("x", "n"), name_test = "GRIM")
#> #' @section Summaries with `audit_total_n()`: You can call 
#> #'   `audit_total_n()` following up on `grim_map_total_n()` 
#> #'   to get a tibble with summary statistics. It will have these columns: 
#> #'  - `x1`, `x2`, and `n` are the original inputs. 
#> #'  - `hits_total` is the number of scenarios in which both 
#> #'  `x1` and `x2` are GRIM-consistent. It is the sum 
#> #'  of `hits_forth` and `hits_back` below. 
#> #'  - `hits_forth` is the number of both-consistent cases that result 
#> #'  from pairing `x2` with the larger dispersed `n` value. 
#> #'  - `hits_back` is the same, except `x1` is 
#> #'  paired with the larger dispersed `n` value. 
#> #'  - `scenarios_total` is the total number of test scenarios, 
#> #'  whether or not both `x1` and `x2` 
#> #'  are GRIM-consistent. 
#> #'  - `hit_rate` is the ratio of `hits_total` to `scenarios_total`. 
#> #' 
#> #'  Call `audit()` following `audit_total_n()` to summarize results 
#> #'  even further. 

# For DEBIT and `debit_map_total_n()`:
write_doc_audit_total_n(key_args = c("x", "sd", "n"), name_test = "DEBIT")
#> #' @section Summaries with `audit_total_n()`: You can call 
#> #'   `audit_total_n()` following up on `debit_map_total_n()` 
#> #'   to get a tibble with summary statistics. It will have these columns: 
#> #'  - `x1`, `x2`, `sd1`, `sd2`, and `n` are the original inputs. 
#> #'  - `hits_total` is the number of scenarios in which all of 
#> #'  `x1`, `x2`, `sd1`, and `sd2` are DEBIT-consistent. It is the sum 
#> #'  of `hits_forth` and `hits_back` below. 
#> #'  - `hits_forth` is the number of both-consistent cases that result 
#> #'  from pairing `x2` and `sd2` with the larger dispersed `n` value. 
#> #'  - `hits_back` is the same, except `x1` and `sd1` are 
#> #'  paired with the larger dispersed `n` value. 
#> #'  - `scenarios_total` is the total number of test scenarios, 
#> #'  whether or not both `x1` and `sd1` as well as `x2` and `sd2` 
#> #'  are DEBIT-consistent. 
#> #'  - `hit_rate` is the ratio of `hits_total` to `scenarios_total`. 
#> #' 
#> #'  Call `audit()` following `audit_total_n()` to summarize results 
#> #'  even further. 
```
