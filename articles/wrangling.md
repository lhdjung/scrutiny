# Data wrangling

Preparing data for error detection can take longer than the tests
themselves: copying numbers from PDF, keeping track of trailing zeros,
or splitting strings that look like `"7.64 (1.5)"`. This vignette gives
some tips for these tasks and presents scrutiny’s wrangling functions.

## Trailing zeros

### Motivation

Numeric values in R don’t preserve trailing zeros. A mean reported as
5.30 is stored as `5.3`, and the data do not show which one it was.
However, trailing zeros are as important to GRIM, GRIMMER, and DEBIT as
any other trailing digits would be.

scrutiny’s consistency tests solve this by asking you to state the
number of decimal places explicitly:
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
has a `digits_x` argument, and
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
and
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
have `digits_x` and `digits_sd`. None of them has a default, so you
can’t forget. Values themselves stay numeric.

To get the trailing zeros back in the values for display or export,
format them as strings with `formatC(x, format = "f", digits = 2)`.

### Enter numbers from PDF

RStudio’s multiple cursors are useful together with
[`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html):

1.  Copy a column of numbers from PDF, pressing and holding `Alt` on
    Windows or `option` on Mac. (This works at least in Adobe Acrobat.)
2.  Paste it into a `tribble()` call as below.
3.  Pressing and holding `Alt`/`option`, select all the copied numbers.
4.  Enter a comma at the end of each, for `tribble()`’s syntax.

You should then get something like this:

``` r

flights1 <- tibble::tribble(
  ~x    ,
   8.97 ,
   2.61 ,
   7.26 ,
   3.64 ,
   9.26 ,
  10.46 ,
   7.39 ,
)
```

Take note of how many decimal places the numbers had in the PDF — here,
two. You will need that number for `digits_x` when you call
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
and the data itself can no longer tell you.

All that’s missing is the sample size. Add it either via another
`tribble()` column as above or via
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html):

``` r

flights1 <- flights1 |>
  dplyr::mutate(n = 28)

flights1
#> # A tibble: 7 × 2
#>       x     n
#>   <dbl> <dbl>
#> 1  8.97    28
#> 2  2.61    28
#> 3  7.26    28
#> 4  3.64    28
#> 5  9.26    28
#> 6 10.46    28
#> 7  7.39    28
```

## Split strings by parentheses

### Basic usage

With summary data copied or extracted from PDF (see above), you might
encounter values presented like `5.22 (0.73)`. Instead of manually
teasing them apart, call
[`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md):

``` r

flights2 <- tibble::tribble(
  ~drone        , ~selfpilot    ,
  "0.09 (0.28)" , "0.19 (0.39)" ,
  "0.19 (0.39)" , "0.53 (0.50)" ,
  "0.62 (0.49)" , "0.5 (0.50)"  ,
  "0.15 (0.31)" , "0.57 (0.50)" ,
)

flights2 |>
  split_by_parens()
#> # A tibble: 4 × 4
#>   drone_x drone_sd selfpilot_x selfpilot_sd
#>   <chr>   <chr>    <chr>       <chr>       
#> 1 0.09    0.28     0.19        0.39        
#> 2 0.19    0.39     0.53        0.50        
#> 3 0.62    0.49     0.5         0.50        
#> 4 0.15    0.31     0.57        0.50
```

With `transform = TRUE`, the output has one row per pair of values
instead, and the column each pair came from is kept in `.origin`:

``` r

flights2 |>
  split_by_parens(transform = TRUE)
#> # A tibble: 8 × 3
#>   .origin   x     sd   
#>   <chr>     <chr> <chr>
#> 1 drone     0.09  0.28 
#> 2 drone     0.19  0.39 
#> 3 drone     0.62  0.49 
#> 4 drone     0.15  0.31 
#> 5 selfpilot 0.19  0.39 
#> 6 selfpilot 0.53  0.50 
#> 7 selfpilot 0.5   0.50 
#> 8 selfpilot 0.57  0.50
```

If these are means and SDs of binary data,
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
can test them almost right away. The `x` and `sd` columns are strings
because they were extracted from strings, so convert them to numeric
first. They still have their trailing zeros at this point, which makes
[`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
a reliable way to count the decimal places of each value. Do this before
converting, and store the counts in `digits_x` and `digits_sd` columns,
which
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
reads if the arguments of these names are not given:

``` r

flights2 |>
  split_by_parens(transform = TRUE) |>
  dplyr::mutate(
    n = 80,
    digits_x = decimal_places(x),
    digits_sd = decimal_places(sd),
    x = as.numeric(x),
    sd = as.numeric(sd)
  ) |>
  debit_map(data_type = "integer")
#> # A tibble: 8 × 8
#> # DEBIT: default settings
#>       x    sd     n data_type digits_x digits_sd consistency .origin  
#>   <dbl> <dbl> <int> <chr>        <int>     <int> <lgl>       <chr>    
#> 1  0.09  0.28    80 integer          2         2 TRUE        drone    
#> 2  0.19  0.39    80 integer          2         2 TRUE        drone    
#> 3  0.62  0.49    80 integer          2         2 TRUE        drone    
#> 4  0.15  0.31    80 integer          2         2 FALSE       drone    
#> 5  0.19  0.39    80 integer          2         2 TRUE        selfpilot
#> 6  0.53  0.5     80 integer          2         2 TRUE        selfpilot
#> 7  0.5   0.5     80 integer          1         2 TRUE        selfpilot
#> 8  0.57  0.5     80 integer          2         2 TRUE        selfpilot
```

Count the decimal places per value, as here, rather than taking the
largest count for a whole column: the mean reported as `"0.5"` would
otherwise be tested as if it were `0.50`, which can make a consistent
value look inconsistent.

For strings like `"2.65 [0.27]"`, set `delim = "brackets"`, and for
`"2.65 {0.27}"`, `delim = "braces"`. Any other pair of delimiters is
given as two strings, such as `delim = c("<", ">")` for `"2.65 <0.27>"`.
The output is the same as for `"2.65 (0.27)"` with the default.

### Column name suffixes

The defaults for column name suffixes are (1) `"x"` for the part before
the parentheses and (2) `"sd"` for the part inside of them. However,
this won’t fit for all data presented like `5.22 (0.73)`. Override the
defaults by specifying `end1` and/or `end2`:

``` r

flights2 |>
  split_by_parens(end1 = "beta", end2 = "se")
#> # A tibble: 4 × 4
#>   drone_beta drone_se selfpilot_beta selfpilot_se
#>   <chr>      <chr>    <chr>          <chr>       
#> 1 0.09       0.28     0.19           0.39        
#> 2 0.19       0.39     0.53           0.50        
#> 3 0.62       0.49     0.5            0.50        
#> 4 0.15       0.31     0.57           0.50
```

These suffixes become column names if `transform` is set to `TRUE`:

``` r

flights2 |>
  split_by_parens(end1 = "beta", end2 = "se", transform = TRUE)
#> # A tibble: 8 × 3
#>   .origin   beta  se   
#>   <chr>     <chr> <chr>
#> 1 drone     0.09  0.28 
#> 2 drone     0.19  0.39 
#> 3 drone     0.62  0.49 
#> 4 drone     0.15  0.31 
#> 5 selfpilot 0.19  0.39 
#> 6 selfpilot 0.53  0.50 
#> 7 selfpilot 0.5   0.50 
#> 8 selfpilot 0.57  0.50
```

### Extract substrings from `before_parens()` and `inside_parens()`

There also are specific functions for extracting the parts of the
individual string vectors before or inside the parentheses:

``` r

flights3 <- flights2 |>
  dplyr::pull(selfpilot)

flights3
#> [1] "0.19 (0.39)" "0.53 (0.50)" "0.5 (0.50)"  "0.57 (0.50)"

flights3 |>
  before_parens()
#> [1] "0.19" "0.53" "0.5"  "0.57"

flights3 |>
  inside_parens()
#> [1] "0.39" "0.50" "0.50" "0.50"
```

## Replace column names by row values

Tables extracted from PDF, as with the tabulapdf package, may come with
nondescript column names, while the correct names are stored in one or
more rows of the data frame itself.

To simulate the problem: `x` and `n` should be column names, but instead
they are values in the first row:

``` r

flights1_with_issues <- flights1 |>
  dplyr::mutate(x = as.character(x), n = as.character(n)) |>
  tibble::add_row(x = "x", n = "n", .before = 1)

colnames(flights1_with_issues) <- c("Var1", "Var2")

flights1_with_issues
#> # A tibble: 8 × 2
#>   Var1  Var2 
#>   <chr> <chr>
#> 1 x     n    
#> 2 8.97  28   
#> 3 2.61  28   
#> 4 7.26  28   
#> 5 3.64  28   
#> 6 9.26  28   
#> 7 10.46 28   
#> 8 7.39  28
```

[`row_to_colnames()`](https://lhdjung.github.io/scrutiny/reference/row_to_colnames.md)
replaces the column names by the values of one or more rows, specified
by position as in
[`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html),
and drops these rows. The default is the first row:

``` r

flights1_with_issues |>
  row_to_colnames()
#> # A tibble: 7 × 2
#>   x     n    
#>   <chr> <chr>
#> 1 8.97  28   
#> 2 2.61  28   
#> 3 7.26  28   
#> 4 3.64  28   
#> 5 9.26  28   
#> 6 10.46 28   
#> 7 7.39  28
```

All columns are still strings, as in any table extracted from PDF. Count
their decimal places with
[`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
while the trailing zeros are still there, then convert them with
[`as.numeric()`](https://rdrr.io/r/base/numeric.html), as in the
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
example above.
