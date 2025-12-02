# Data wrangling

``` r
library(scrutiny)
```

In general, scrutiny’s techniques for error detection are designed for a
focus on the essential points, cutting out time-consuming repetition.
There are some bottlenecks, however, such as entering decimal numbers as
strings, or splitting strings that look like `"7.64 (1.5)"`.

This vignette shows how to save your time preparing data for error
detection. It gives some general tips for these tasks, and then presents
scrutiny’s own specialized wrangling functions.

## Trailing zeros

### Motivation

One particular challenge when looking for numeric irregularities using R
is that numbers often have to be treated as strings. The reason is that
numeric values don’t preserve any trailing zeros. This is a major
problem because trailing zeros are as important to, e.g., GRIM or DEBIT
as any other trailing digits would be.

The only solution I know of is to work with strings — namely, strings
that can be converted to non-`NA` numeric values. I will discuss two
ways to work with them: (1) directly entering or importing numbers as
strings, and (2) restoring trailing zeros.

### Enter numbers as strings

#### Automated

Several R packages help to extract tables from PDF. I recommend
tabulizer (not currently on CRAN; see [installation
notes](https://stackoverflow.com/questions/70036429/having-issues-installing-tabulizer-package-in-r)).
There are also the
[pdftables](https://expersso.r-universe.dev/pdftables#) and
[pdftools](https://ropensci.r-universe.dev/pdftools#) packages.

Using tabulizer requires Java to be installed. When it works well,
tabulizer is a great tool for importing tables quickly and efficiently.
It automatically captures values as strings, so trailing zeros are
treated just like other digits.

However, tabulizer might sometimes struggle, especially with older PDF
files. That is most likely the fault of the PDF format itself because it
has no inbuilt support for tables, so any effort to extract them faces
serious ambiguities. (See below, *Replace column names by row values*,
for a solution to one such issue.)

If there are many tables in multiple files formatted in the same way, it
can be useful to check if tabulizer reliably and accurately captures
them. If it doesn’t, you might have to use copy and paste.

#### With copy and paste

Perhaps not all R users know that RStudio features an option for
multiple cursors. These are especially useful in conjunction with
[`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html),
which is available via scrutiny. Here’s how to use multiple cursors in
the present context:

1.  Copy a column of numbers from PDF, pressing and holding `Alt` on
    Windows or `option` on Mac. (This works at least in Adobe Acrobat.)
2.  Paste it into a `tribble()` call as below.
3.  Pressing and holding `Alt`/`option`, select all the copied numbers.
4.  Enter quotation marks and, for `tribble()`’s syntax, a comma.

You should then get something like this:

``` r
flights1 <- tibble::tribble(
  ~x,
"8.97",
"2.61",
"7.26",
"3.64",
"9.26",
"10.46",
"7.39",
)
```

All that’s missing is the sample size. Add it either via another
`tribble()` column as above or via
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
which also comes with scrutiny:

``` r
flights1 <- flights1 %>% 
  dplyr::mutate(n = 28)

flights1
#> # A tibble: 7 × 2
#>   x         n
#>   <chr> <dbl>
#> 1 8.97     28
#> 2 2.61     28
#> 3 7.26     28
#> 4 3.64     28
#> 5 9.26     28
#> 6 10.46    28
#> 7 7.39     28
```

### Restore trailing zeros

When dealing with numbers that used to have trailing zeros but lost them
from being registered as numeric, call
[`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
to format them correctly. Suppose all of the following numbers
originally had one decimal place, but some no longer do:

``` r
vec <- c(4, 6.9, 5, 4.2, 4.8, 7, 4)

vec %>% 
  decimal_places()
#> [1] 0 1 0 1 1 0 0
```

Now, get them back with
[`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md):

``` r
vec %>% 
  restore_zeros()
#> [1] "4.0" "6.9" "5.0" "4.2" "4.8" "7.0" "4.0"

vec %>% 
  restore_zeros() %>% 
  decimal_places()
#> [1] 1 1 1 1 1 1 1
```

This uses the default of going by the longest mantissa and padding the
other strings with decimal zeros until they have that many decimal
places. However, this is just a heuristic: The longest mantissa might
itself have lost decimal places. Specify the `width` argument to
explicitly state the desired mantissa length:

``` r
vec %>% 
  restore_zeros(width = 2)
#> [1] "4.00" "6.90" "5.00" "4.20" "4.80" "7.00" "4.00"

vec %>% 
  restore_zeros(width = 2) %>% 
  decimal_places()
#> [1] 2 2 2 2 2 2 2
```

A convenient way to restore trailing zeros in a data frame is
[`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md).
By default, it operates on all columns that are coercible to numeric
(factors don’t count):

``` r
iris <- tibble::as_tibble(iris)
iris %>% 
  restore_zeros_df(width = 3)
#> # A tibble: 150 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>    <chr>        <chr>       <chr>        <chr>       <fct>  
#>  1 5.100        3.500       1.400        0.200       setosa 
#>  2 4.900        3.000       1.400        0.200       setosa 
#>  3 4.700        3.200       1.300        0.200       setosa 
#>  4 4.600        3.100       1.500        0.200       setosa 
#>  5 5.000        3.600       1.400        0.200       setosa 
#>  6 5.400        3.900       1.700        0.400       setosa 
#>  7 4.600        3.400       1.400        0.300       setosa 
#>  8 5.000        3.400       1.500        0.200       setosa 
#>  9 4.400        2.900       1.400        0.200       setosa 
#> 10 4.900        3.100       1.500        0.100       setosa 
#> # ℹ 140 more rows
```

Specify columns mostly like you would in
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html):

``` r
iris %>% 
  restore_zeros_df(starts_with("Sepal"), width = 3)
#> # A tibble: 150 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>    <chr>        <chr>              <dbl>       <dbl> <fct>  
#>  1 5.100        3.500                1.4         0.2 setosa 
#>  2 4.900        3.000                1.4         0.2 setosa 
#>  3 4.700        3.200                1.3         0.2 setosa 
#>  4 4.600        3.100                1.5         0.2 setosa 
#>  5 5.000        3.600                1.4         0.2 setosa 
#>  6 5.400        3.900                1.7         0.4 setosa 
#>  7 4.600        3.400                1.4         0.3 setosa 
#>  8 5.000        3.400                1.5         0.2 setosa 
#>  9 4.400        2.900                1.4         0.2 setosa 
#> 10 4.900        3.100                1.5         0.1 setosa 
#> # ℹ 140 more rows
```

## Split strings by parentheses

### Basic usage

With summary data copied or extracted from PDF (see above), you might
encounter values presented like `5.22 (0.73)`. Instead of manually
teasing them apart, call
[`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md):

``` r
flights2 <- tibble::tribble(
  ~drone,           ~selfpilot,
  "0.09 (0.21)",    "0.19 (0.13)",
  "0.19 (0.28)",    "0.53 (0.10)",
  "0.62 (0.16)",    "0.50 (0.11)",
  "0.15 (0.35)",    "0.57 (0.16)",
)

flights2 %>% 
  split_by_parens()
#> # A tibble: 4 × 4
#>   drone_x drone_sd selfpilot_x selfpilot_sd
#>   <chr>   <chr>    <chr>       <chr>       
#> 1 0.09    0.21     0.19        0.13        
#> 2 0.19    0.28     0.53        0.10        
#> 3 0.62    0.16     0.50        0.11        
#> 4 0.15    0.35     0.57        0.16
```

Optionally, transform these values into a more useful format:

``` r
flights2 %>% 
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
```

From here, you can call
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
almost right away (supposing you deal with binary distributions’ means
and standard deviations):

``` r
flights2 %>% 
  split_by_parens(transform = TRUE) %>% 
  dplyr::mutate(n = 80) %>% 
  debit_map()
#> # A tibble: 8 × 12
#>   x     sd        n consistency rounding   sd_lower sd_incl_lower sd_upper
#>   <chr> <chr> <int> <lgl>       <chr>         <dbl> <lgl>            <dbl>
#> 1 0.09  0.21     80 FALSE       up_or_down    0.205 TRUE             0.215
#> 2 0.19  0.28     80 FALSE       up_or_down    0.275 TRUE             0.285
#> 3 0.62  0.16     80 FALSE       up_or_down    0.155 TRUE             0.165
#> 4 0.15  0.35     80 TRUE        up_or_down    0.345 TRUE             0.355
#> 5 0.19  0.13     80 FALSE       up_or_down    0.125 TRUE             0.135
#> 6 0.53  0.10     80 FALSE       up_or_down    0.095 TRUE             0.105
#> 7 0.50  0.11     80 FALSE       up_or_down    0.105 TRUE             0.115
#> 8 0.57  0.16     80 FALSE       up_or_down    0.155 TRUE             0.165
#> # ℹ 4 more variables: sd_incl_upper <lgl>, x_lower <dbl>, x_upper <dbl>,
#> #   .origin <chr>
```

If your strings look like `"2.65 [0.27]"`, specify the `sep` argument as
`"brackets"`. Likewise for `"2.65 {0.27}"` and `sep = "braces"`. What
about other separators, as in `"2.65 <0.27>"`? Specify `sep` as those
two substrings, like `sep = c("<", ">")`. In all of these cases, the
output will be the same as the default would be if the strings were like
`"2.65 (0.27)"`.

### Column name suffixes

The defaults for column name suffixes are (1) `"x"` for the part before
the parentheses and (2) `"sd"` for the part inside of them. However,
this won’t fit for all data presented like `5.22 (0.73)`. Override the
defaults by specifying `col1` and/or `col2`:

``` r
flights2 %>% 
  split_by_parens(end1 = "beta", end2 = "se")
#> # A tibble: 4 × 4
#>   drone_beta drone_se selfpilot_beta selfpilot_se
#>   <chr>      <chr>    <chr>          <chr>       
#> 1 0.09       0.21     0.19           0.13        
#> 2 0.19       0.28     0.53           0.10        
#> 3 0.62       0.16     0.50           0.11        
#> 4 0.15       0.35     0.57           0.16
```

These suffixes become column names if `transform` is set to `TRUE`:

``` r
flights2 %>% 
  split_by_parens(end1 = "beta", end2 = "se", transform = TRUE)
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
```

### Extract substrings from `before_parens()` and `inside_parens()`

There also are specific functions for extracting the parts of the
individual string vectors before or inside the parentheses:

``` r
flights3 <- flights2 %>% 
  dplyr::pull(selfpilot)

flights3
#> [1] "0.19 (0.13)" "0.53 (0.10)" "0.50 (0.11)" "0.57 (0.16)"

flights3 %>% 
  before_parens()
#> [1] "0.19" "0.53" "0.50" "0.57"

flights3 %>% 
  inside_parens()
#> [1] "0.13" "0.10" "0.11" "0.16"
```

## Replace column names by row values

When extracting tables from PDF with tabulizer, you might get data
frames (converted from matrices) that have wrong, nondescript column
names, while the correct column names are stored in one or more rows
within the data frame itself.

I will first simulate the problem. `x` and `n` should be column names,
but instead they are values in the first row:

``` r
flights1_with_issues <- flights1 %>% 
    dplyr::mutate(n = as.character(n)) %>% 
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

To remedy the issue, call
[`row_to_colnames()`](https://lhdjung.github.io/scrutiny/reference/row_to_colnames.md)
on the data frame. It will replace the column names by the values of one
or more rows. The latter are specified by their position numbers as in
[`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html).
For these numbers, the default is `1` because the column names will
often be stored in the first row, if at all. The specified row or rows
are then dropped because they shouldn’t have been rows in the first
place.

With the above example:

``` r
flights1_with_issues %>% 
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

Note that `n` is still a string vector, but this is true for all columns
in tables extracted with tabulizer.
