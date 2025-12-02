# Implementing your consistency test

``` r
library(scrutiny)
```

Use scrutiny to implement new consistency tests in R. Consistency tests,
such as GRIM, are procedures that check whether two or more summary
values can describe the same data.

This vignette shows you the minimal steps required to tap into
scrutiny’s framework for implementing consistency tests. The key idea is
to focus on the core logic of your test and let scrutiny’s functions
take care of iteration. For an in-depth treatment, see
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md).

## 1. Single-case

Encode the logic of your test in a simple function that takes single
values. It should return `TRUE` if they are consistent and `FALSE` if
they are not. Its name should end on `_scalar`, which refers to its
single-case nature. Here, I use a mock test without real meaning, called
SCHLIM:

``` r
schlim_scalar <- function(y, n) {
  y <- as.numeric(y)
  n <- as.numeric(n)
  all(y / 3 > n)
}

schlim_scalar(y = 30, n = 4)
#> [1] TRUE
schlim_scalar(y = 2, n = 7)
#> [1] FALSE
```

## 2. Vectorized

For completeness, although it’s not very important in practice —
[`Vectorize()`](https://rdrr.io/r/base/Vectorize.html) from base R helps
you turn the single-case function into a vectorized one, so that the new
function’s arguments can have a length greater than 1:

``` r
schlim <- Vectorize(schlim_scalar)

schlim(y = 10:15, n = 4)
#> [1] FALSE FALSE FALSE  TRUE  TRUE  TRUE
```

## 3. Basic mapper

Next, create a function that tests many values in a data frame, like
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
does. Its name should also end on `_map`. Use
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
to get this function without much effort:

``` r
schlim_map <- function_map(
  .fun = schlim_scalar,
  .reported = c("y", "n"),
  .name_test = "SCHLIM"
)

# Example data:
df1 <- tibble::tibble(y = 16:25, n = 3:12)

schlim_map(df1)
#> # A tibble: 10 × 3
#>        y     n consistency
#>    <int> <int> <lgl>      
#>  1    16     3 TRUE       
#>  2    17     4 TRUE       
#>  3    18     5 TRUE       
#>  4    19     6 TRUE       
#>  5    20     7 FALSE      
#>  6    21     8 FALSE      
#>  7    22     9 FALSE      
#>  8    23    10 FALSE      
#>  9    24    11 FALSE      
#> 10    25    12 FALSE
```

## 4. `audit()` method

Use scrutiny’s
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
generic to get summary statistics. Write a new function named
`audit.scr_name_map()`, where `name` is the name of your test in
lower-case — here, `schlim`.

Within the function body, call
[`audit_cols_minimal()`](https://lhdjung.github.io/scrutiny/reference/audit_cols_minimal.md).
This enables you to use
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
following the mapper function:

``` r
audit.scr_schlim_map <- function(data) {
  audit_cols_minimal(data, name_test = "SCHLIM")
}

df1 %>% 
  schlim_map() %>% 
  audit()
#> # A tibble: 1 × 3
#>   incons_cases all_cases incons_rate
#>          <int>     <int>       <dbl>
#> 1            6        10         0.6
```

[`audit_cols_minimal()`](https://lhdjung.github.io/scrutiny/reference/audit_cols_minimal.md)
only provides the most basic summaries. If you like, you can still add
summary statistics that are more specific to your test. See, e.g., the
*Summaries with
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)*
section in
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)’s
documentation.

## 5. Sequence mapper

This kind of mapper function tests hypothetical values around the
reported ones, like
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
does. Create a sequence mapper by simply calling
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md):

``` r
schlim_map_seq <- function_map_seq(
  .fun = schlim_map,
  .reported = c("y", "n"),
  .name_test = "SCHLIM"
)

df1 %>% 
  schlim_map_seq()
#> # A tibble: 120 × 6
#>        y     n consistency diff_var  case var  
#>    <int> <int> <lgl>          <int> <int> <chr>
#>  1    15     7 FALSE             -5     1 y    
#>  2    16     7 FALSE             -4     1 y    
#>  3    17     7 FALSE             -3     1 y    
#>  4    18     7 FALSE             -2     1 y    
#>  5    19     7 FALSE             -1     1 y    
#>  6    21     7 FALSE              1     1 y    
#>  7    22     7 TRUE               2     1 y    
#>  8    23     7 TRUE               3     1 y    
#>  9    24     7 TRUE               4     1 y    
#> 10    25     7 TRUE               5     1 y    
#> # ℹ 110 more rows
```

Get summary statistics with
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md):

``` r
df1 %>% 
  schlim_map_seq() %>% 
  audit_seq()
#> # A tibble: 6 × 12
#>       y     n consistency hits_total hits_y hits_n diff_y diff_y_up diff_y_down
#>   <int> <int> <lgl>            <int>  <int>  <int>  <int>     <int>       <int>
#> 1    20     7 FALSE                9      4      5      2         2          NA
#> 2    21     8 FALSE                6      2      4      4         4          NA
#> 3    22     9 FALSE                4      0      4     NA        NA          NA
#> 4    23    10 FALSE                3      0      3     NA        NA          NA
#> 5    24    11 FALSE                2      0      2     NA        NA          NA
#> 6    25    12 FALSE                2      0      2     NA        NA          NA
#> # ℹ 3 more variables: diff_n <int>, diff_n_up <int>, diff_n_down <int>
```

## 6. Total-n mapper

Suppose you have grouped data but no group sizes are known, only a total
sample size:

``` r
df2 <- tibble::tribble(
  ~y1, ~y2, ~n,
   84,  37,  29,
   61,  55,  26
)
```

To tackle this, create a total-n mapper that varies hypothetical group
sizes:

``` r
schlim_map_total_n <- function_map_total_n(
  .fun = schlim_map,
  .reported = "y",
  .name_test = "SCHLIM"
)

df2 %>% 
  schlim_map_total_n()
#> # A tibble: 48 × 7
#>        y     n n_change consistency both_consistent  case dir  
#>    <dbl> <int>    <int> <lgl>       <lgl>           <int> <fct>
#>  1    84    14        0 TRUE        FALSE               1 forth
#>  2    37    15        0 FALSE       FALSE               1 forth
#>  3    84    13       -1 TRUE        FALSE               1 forth
#>  4    37    16        1 FALSE       FALSE               1 forth
#>  5    84    12       -2 TRUE        FALSE               1 forth
#>  6    37    17        2 FALSE       FALSE               1 forth
#>  7    84    11       -3 TRUE        FALSE               1 forth
#>  8    37    18        3 FALSE       FALSE               1 forth
#>  9    84    10       -4 TRUE        FALSE               1 forth
#> 10    37    19        4 FALSE       FALSE               1 forth
#> # ℹ 38 more rows
```

Get summary statistics with
[`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md):

``` r
df2 %>% 
  schlim_map_total_n() %>% 
  audit_total_n()
#> # A tibble: 2 × 8
#>      y1    y2     n hits_total hits_forth hits_back scenarios_total hit_rate
#>   <dbl> <dbl> <int>      <int>      <int>     <int>           <int>    <dbl>
#> 1    84    37    29          4          0         4              12    0.333
#> 2    61    55    26         12          6         6              12    1
```
