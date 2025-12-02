# Split columns by parentheses, brackets, braces, or similar

Summary statistics are often presented like `"2.65 (0.27)"`. When
working with tables copied into R, it can be tedious to separate values
before and inside parentheses. `split_by_parens()` does this
automatically.

By default, it operates on all columns. Output can optionally be pivoted
into a longer format by setting `transform` to `TRUE`.

Choose separators other than parentheses with the `sep` argument.

## Usage

``` r
split_by_parens(
  data,
  cols = everything(),
  check_sep = TRUE,
  keep = FALSE,
  transform = FALSE,
  sep = "parens",
  end1 = "x",
  end2 = "sd",
  ...
)
```

## Arguments

- data:

  Data frame.

- cols:

  Select columns from `data` using
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html).
  Default is `everything()`, which selects all columns that pass
  `check_sep`.

- check_sep:

  Logical. If `TRUE` (the default), columns are excluded if they don't
  contain the `sep` elements.

- keep:

  Logical. If set to `TRUE`, the originally selected columns that were
  split by the function also appear in the output. Default is `FALSE`.

- transform:

  Logical. If set to `TRUE`, the output will be pivoted to be better
  suitable for typical follow-up tasks. Default is `FALSE`.

- sep:

  String. What to split by. Either `"parens"`, `"brackets"`, or
  `"braces"`; or a length-2 vector of custom separators (see Examples).
  Default is `"parens"`.

- end1, end2:

  Strings. Endings of the two column names that result from splitting a
  column. Default is `"x"` for `end1` and `"sd"` for `end2`.

- ...:

  These dots must be empty.

## Value

Data frame.

## See also

- [`before_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md)
  and
  [`inside_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md)
  take a string vector and extract values from the respective position.

- [`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html)
  powers the application of the two above functions within
  `split_by_parens()`, including the creation of new columns.

- [`tidyr::separate_wider_delim()`](https://tidyr.tidyverse.org/reference/separate_wider_delim.html)
  is a more general function, but it does not recognize closing elements
  such as closed parentheses.

## Examples

``` r
# Call `split_by_parens()` on data like these:
df1 <- tibble::tribble(
  ~drone,           ~selfpilot,
  "0.09 (0.21)",    "0.19 (0.13)",
  "0.19 (0.28)",    "0.53 (0.10)",
  "0.62 (0.16)",    "0.50 (0.11)",
  "0.15 (0.35)",    "0.57 (0.16)",
)

# Basic usage:
df1 %>%
  split_by_parens()
#> # A tibble: 4 × 4
#>   drone_x drone_sd selfpilot_x selfpilot_sd
#>   <chr>   <chr>    <chr>       <chr>       
#> 1 0.09    0.21     0.19        0.13        
#> 2 0.19    0.28     0.53        0.10        
#> 3 0.62    0.16     0.50        0.11        
#> 4 0.15    0.35     0.57        0.16        

# Name specific columns with `cols` to only split those:
df1 %>%
  split_by_parens(cols = drone)
#> # A tibble: 4 × 3
#>   drone_x drone_sd selfpilot  
#>   <chr>   <chr>    <chr>      
#> 1 0.09    0.21     0.19 (0.13)
#> 2 0.19    0.28     0.53 (0.10)
#> 3 0.62    0.16     0.50 (0.11)
#> 4 0.15    0.35     0.57 (0.16)

# Pivot the data into a longer format
# by setting `transform` to `TRUE`:
df1 %>%
  split_by_parens(transform = TRUE)
#> # A tibble: 8 × 3
#>   .origin   x     sd   
#>   <chr>     <chr> <chr>
#> 1 drone     0.09  0.21 
#> 2 drone     0.19  0.28 
#> 3 drone     0.62  0.16 
#> 4 drone     0.15  0.35 
#> 5 selfpilot 0.19  0.13 
#> 6 selfpilot 0.53  0.10 
#> 7 selfpilot 0.50  0.11 
#> 8 selfpilot 0.57  0.16 

# Choose different column names or
# name suffixes with `end1` and `end2`:
df1 %>%
  split_by_parens(end1 = "beta", end2 = "se")
#> # A tibble: 4 × 4
#>   drone_beta drone_se selfpilot_beta selfpilot_se
#>   <chr>      <chr>    <chr>          <chr>       
#> 1 0.09       0.21     0.19           0.13        
#> 2 0.19       0.28     0.53           0.10        
#> 3 0.62       0.16     0.50           0.11        
#> 4 0.15       0.35     0.57           0.16        

df1 %>%
  split_by_parens(
    transform = TRUE,
    end1 = "beta", end2 = "se"
  )
#> # A tibble: 8 × 3
#>   .origin   beta  se   
#>   <chr>     <chr> <chr>
#> 1 drone     0.09  0.21 
#> 2 drone     0.19  0.28 
#> 3 drone     0.62  0.16 
#> 4 drone     0.15  0.35 
#> 5 selfpilot 0.19  0.13 
#> 6 selfpilot 0.53  0.10 
#> 7 selfpilot 0.50  0.11 
#> 8 selfpilot 0.57  0.16 

# With a different separator...
df2 <- tibble::tribble(
  ~drone,           ~selfpilot,
  "0.09 [0.21]",    "0.19 [0.13]",
  "0.19 [0.28]",    "0.53 [0.10]",
  "0.62 [0.16]",    "0.50 [0.11]",
  "0.15 [0.35]",    "0.57 [0.16]",
)

# ... specify `sep`:
df2 %>%
  split_by_parens(sep = "brackets")
#> # A tibble: 4 × 4
#>   drone_x drone_sd selfpilot_x selfpilot_sd
#>   <chr>   <chr>    <chr>       <chr>       
#> 1 0.09    0.21     0.19        0.13        
#> 2 0.19    0.28     0.53        0.10        
#> 3 0.62    0.16     0.50        0.11        
#> 4 0.15    0.35     0.57        0.16        

# (Accordingly with `{}` and `"braces"`.)

# If the separator is yet a different one...
df3 <- tibble::tribble(
  ~drone,           ~selfpilot,
  "0.09 <0.21>",    "0.19 <0.13>",
  "0.19 <0.28>",    "0.53 <0.10>",
  "0.62 <0.16>",    "0.50 <0.11>",
  "0.15 <0.35>",    "0.57 <0.16>",
)

# ... `sep` should be a length-2 vector
# that contains the separating elements:
df3 %>%
  split_by_parens(sep = c("<", ">"))
#> # A tibble: 4 × 4
#>   drone_x drone_sd selfpilot_x selfpilot_sd
#>   <chr>   <chr>    <chr>       <chr>       
#> 1 0.09    0.21     0.19        0.13        
#> 2 0.19    0.28     0.53        0.10        
#> 3 0.62    0.16     0.50        0.11        
#> 4 0.15    0.35     0.57        0.16        
```
