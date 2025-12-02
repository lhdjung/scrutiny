# Documentation template for [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)

`write_doc_audit_seq()` creates a roxygen2 block section to be inserted
into the documentation of functions created with
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md).
The section informs users about the ways in which
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
summarizes the results of the manufactured `*_map_seq()` function.

Copy the output from your console and paste it into the roxygen2 block
of your `*_map_seq()` function. To preserve the bullet-point structure
when indenting roxygen2 comments with `Ctrl`+`Shift`+`/`, leave empty
lines between the pasted output and the rest of the block.

## Usage

``` r
write_doc_audit_seq(key_args, name_test)
```

## Arguments

- key_args:

  String vector with the names of the key columns that are tested for
  consistency by the `*_map_seq()` function. The values need to have the
  same order as in that function's output.

- name_test:

  String (length 1). Name of the consistency test which the
  `*_map_seq()` function applies, such as `"GRIM"`.

## Value

A string vector formatted by
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

## See also

The sister function
[`write_doc_audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/write_doc_audit_total_n.md)
and, for context,
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md).

## Examples

``` r
# For GRIM and `grim_map_seq()`:
write_doc_audit_seq(key_args = c("x", "n"), name_test = "GRIM")
#> #' @section Summaries with `audit_seq()`: You can call `audit_seq()` following 
#> #'   `grim_map_seq()`. It will return a data frame with these columns: 
#> #'   - `x` and `n` are the original inputs, 
#> #'   tested for `consistency` here. 
#> #'   - `hits_total` is the total number of GRIM-consistent value sets 
#> #'   found within the specified `dispersion` range. 
#> #'   - `hits_x` is the number of GRIM-consistent value sets 
#> #'   found by varying `x`. 
#> #'   - Accordingly with `n` and `hits_n`. 
#> #'   - (Note that any consistent reported cases will be counted by the 
#> #'   `hits_*` columns if both `include_reported` and `include_consistent` 
#> #'   are set to `TRUE`.) 
#> #'   - `diff_x` reports the absolute difference between `x` and the next 
#> #'   consistent dispersed value (in dispersion steps, not the actual numeric 
#> #'   difference). `diff_x_up` and `diff_x_down` report the difference to the 
#> #'   next higher or lower consistent value, respectively. 
#> #'   - `diff_n`, `diff_n_up`, and `diff_n_down` do the same for `n`. 
#> #' 
#> #'   Call `audit()` following `audit_seq()` to summarize results even further. 
#> #'   It's mostly self-explaining, but `na_count` and `na_rate` are the number 
#> #'   and rate of times that a difference could not be computed because of a lack 
#> #'   of corresponding hits within the `dispersion` range. 

# For DEBIT and `debit_map_seq()`:
write_doc_audit_seq(key_args = c("x", "sd", "n"), name_test = "DEBIT")
#> #' @section Summaries with `audit_seq()`: You can call `audit_seq()` following 
#> #'   `debit_map_seq()`. It will return a data frame with these columns: 
#> #'   - `x`, `sd`, and `n` are the original inputs, 
#> #'   tested for `consistency` here. 
#> #'   - `hits_total` is the total number of DEBIT-consistent value sets 
#> #'   found within the specified `dispersion` range. 
#> #'   - `hits_x` is the number of DEBIT-consistent value sets 
#> #'   found by varying `x`. 
#> #'   - Accordingly with `sd` and `hits_sd` as well as `n` and `hits_n`. 
#> #'   - (Note that any consistent reported cases will be counted by the 
#> #'   `hits_*` columns if both `include_reported` and `include_consistent` 
#> #'   are set to `TRUE`.) 
#> #'   - `diff_x` reports the absolute difference between `x` and the next 
#> #'   consistent dispersed value (in dispersion steps, not the actual numeric 
#> #'   difference). `diff_x_up` and `diff_x_down` report the difference to the 
#> #'   next higher or lower consistent value, respectively. 
#> #'   - `diff_sd`, `diff_sd_up`, and `diff_sd_down` do the same for `sd`. 
#> #'   -  Likewise with `diff_n`, `diff_n_up`, and `diff_n_down`. 
#> #' 
#> #'   Call `audit()` following `audit_seq()` to summarize results even further. 
#> #'   It's mostly self-explaining, but `na_count` and `na_rate` are the number 
#> #'   and rate of times that a difference could not be computed because of a lack 
#> #'   of corresponding hits within the `dispersion` range. 
```
