# Split columns by parentheses, brackets, braces, or similar

Summary statistics are often reported like `"2.65 (0.27)"`.
`split_by_parens()` splits such columns into the part before the
parentheses and the part inside them. By default, it splits every column
that contains the delimiters. Set `transform` to `TRUE` to pivot the
result into a longer format.

## Usage

``` r
split_by_parens(
  data,
  cols = everything(),
  check_delim = TRUE,
  keep = FALSE,
  transform = FALSE,
  delim = "parens",
  end1 = "x",
  end2 = "sd",
  check_sep = deprecated(),
  sep = deprecated()
)
```

## Arguments

- data:

  Data frame or matrix.

- cols:

  Columns to split, selected with
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html).
  Default is `everything()`, restricted by `check_delim`.

- check_delim:

  Logical. If `TRUE` (the default), columns whose values do not all
  contain the delimiters are skipped. Missing values, blank strings,
  `"NA"`, and `"NaN"` don't count. If `cols` is given, each column
  skipped causes a warning. Under `check_delim = FALSE`, so does each
  column split although it lacks the delimiters, unless all its values
  are missing.

- keep:

  Logical. Should the original columns be kept? Default is `FALSE`.

- transform:

  Logical. Should the output be pivoted into a longer format, with one
  column per part and an `.origin` column naming the source? Default is
  `FALSE`. Requires `keep = FALSE` and every column to be split.

- delim:

  String. The pair of delimiters around the second part: `"parens"` (the
  default), `"brackets"`, or `"braces"`, or two custom strings, matched
  literally.

- end1, end2:

  Strings. Suffixes of the two columns that a split column produces.
  Defaults are `"x"` and `"sd"`.

- check_sep, sep:

  **\[deprecated\]** The old names of `check_delim` and `delim`.

## Value

An ungrouped data frame: a tibble unless `data` is a plain data frame
and `transform` is `FALSE`.

## See also

- [`before_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md)
  and
  [`inside_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md)
  for the two parts of a string vector.

- [`tidyr::separate_wider_delim()`](https://tidyr.tidyverse.org/reference/separate_wider_delim.html),
  which is more general but does not recognize closing delimiters.

## Examples

``` r
df1 <- tibble::tribble(
  ~drone,           ~selfpilot,
  "0.09 (0.21)",    "0.19 (0.13)",
  "0.19 (0.28)",    "0.53 (0.10)",
  "0.62 (0.16)",    "0.50 (0.11)",
  "0.15 (0.35)",    "0.57 (0.16)",
)

df1 |>
  split_by_parens()
#> # A tibble: 4 × 4
#>   drone_x drone_sd selfpilot_x selfpilot_sd
#>   <chr>   <chr>    <chr>       <chr>       
#> 1 0.09    0.21     0.19        0.13        
#> 2 0.19    0.28     0.53        0.10        
#> 3 0.62    0.16     0.50        0.11        
#> 4 0.15    0.35     0.57        0.16        

# Only some columns:
df1 |>
  split_by_parens(cols = drone)
#> # A tibble: 4 × 3
#>   drone_x drone_sd selfpilot  
#>   <chr>   <chr>    <chr>      
#> 1 0.09    0.21     0.19 (0.13)
#> 2 0.19    0.28     0.53 (0.10)
#> 3 0.62    0.16     0.50 (0.11)
#> 4 0.15    0.35     0.57 (0.16)

# Longer format:
df1 |>
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

# Other suffixes:
df1 |>
  split_by_parens(end1 = "beta", end2 = "se")
#> # A tibble: 4 × 4
#>   drone_beta drone_se selfpilot_beta selfpilot_se
#>   <chr>      <chr>    <chr>          <chr>       
#> 1 0.09       0.21     0.19           0.13        
#> 2 0.19       0.28     0.53           0.10        
#> 3 0.62       0.16     0.50           0.11        
#> 4 0.15       0.35     0.57           0.16        

# Other delimiters:
df2 <- tibble::tribble(
  ~drone,           ~selfpilot,
  "0.09 [0.21]",    "0.19 [0.13]",
  "0.19 [0.28]",    "0.53 [0.10]",
)

df2 |>
  split_by_parens(delim = "brackets")
#> # A tibble: 2 × 4
#>   drone_x drone_sd selfpilot_x selfpilot_sd
#>   <chr>   <chr>    <chr>       <chr>       
#> 1 0.09    0.21     0.19        0.13        
#> 2 0.19    0.28     0.53        0.10        

df3 <- tibble::tribble(
  ~drone,           ~selfpilot,
  "0.09 <0.21>",    "0.19 <0.13>",
  "0.19 <0.28>",    "0.53 <0.10>",
)

df3 |>
  split_by_parens(delim = c("<", ">"))
#> # A tibble: 2 × 4
#>   drone_x drone_sd selfpilot_x selfpilot_sd
#>   <chr>   <chr>    <chr>       <chr>       
#> 1 0.09    0.21     0.19        0.13        
#> 2 0.19    0.28     0.53        0.10        
```
