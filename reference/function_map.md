# Create new `*_map()` functions

`function_map()` creates new basic mapper functions for consistency
tests, such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
or
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md).

For context, see [*Creating basic mappers with
`function_map()`*](https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#creating-basic-mappers-with-function_map).

## Usage

``` r
function_map(
  .fun,
  .reported,
  .name_test,
  .name_key_result = "consistency",
  .name_class = NULL,
  .args_disabled = NULL,
  .col_names = NULL,
  .col_control = NULL,
  .col_filler = NULL,
  ...
)
```

## Arguments

- .fun:

  Single-case consistency testing function that will be applied to each
  row in a data frame. The function must return a single logical value,
  i.e., `TRUE`, `FALSE`, or `NA`.

- .reported:

  String. Names of the columns to be tested.

- .name_test:

  String (length 1). Plain-text name of the consistency test, such as
  `"GRIM"`.

- .name_key_result:

  (Experimental) Optionally, a single string that will be the name of
  the key result column in the output. Default is `"consistency"`.

- .name_class:

  String. Optionally, one or more classes to be added to the output data
  frame. Default is `NULL`, i.e., no extra class (but see *Details*).

- .args_disabled:

  Optionally, a string vector with names of arguments of the
  `*_scalar()` function that don't work with the factory-made function.
  If the user tries to specify these arguments, an informative error
  will be thrown.

- .col_names:

  (Experimental) Optionally, a string vector with the names of
  additional columns that are derived from the `*_scalar()` function.
  Requires `.col_control` and `.col_filler` specifications.

- .col_control:

  (Experimental) Optionally, a single string with the name of the
  `*_scalar()` function's logical argument that controls if the columns
  named in `.col_names` will be displayed.

- .col_filler:

  (Experimental) Optionally, a vector specifying the values of
  `.col_names` columns in rows where the `*_scalar()` function only
  returned the `consistency` value.

- ...:

  These dots must be empty.

## Value

A factory-made function with these arguments:

- `data`: Data frame with all the columns named in `.reported`. It must
  have columns named after the key arguments in `.fun`. Other columns
  are permitted.

- Arguments named after the `.reported` values. They can be specified as
  the names of `data` columns so that the function will rename that
  column using the `.reported` name.

- `reported`, `fun`, `name_class`: Same as when calling `function_map()`
  but spelled without dots. You can override these defaults when calling
  the factory-made function.

- `...`: Arguments passed down to `.fun`. This does not include the
  column-identifying arguments derived from `.reported`.

## Details

The output tibble returned by the factory-made function will inherit one
or two classes independently of the `.name_class` argument:

- It will inherit a class named `"scr_{tolower(.name_test)}_map"`; for
  example, the class is `"scr_grim_map"` if `.name_test` is `"GRIM"`.

- If a `rounding` argument is specified via `...`, or else if `.fun` has
  a `rounding` argument with a default, the output tibble will inherit a
  class named `"scr_rounding_{rounding}"`; for example,
  `"scr_rounding_up_or_down"`.

## Value returned by the factory-made function

A tibble that includes `"consistency"`: a logical column showing whether
the values to its left are mutually consistent (`TRUE`) or not
(`FALSE`).

## Examples

``` r
# Basic test implementation for "SCHLIM",
# a mock test with no real significance:
schlim_scalar <- function(y, n) {
  (y / 3) > n
}

# Let the function factory produce
# a mapper function for SCHLIM:
schlim_map <- function_map(
  .fun = schlim_scalar,
  .reported = c("y", "n"),
  .name_test = "SCHLIM"
)

# Example data:
df1 <- tibble::tibble(y = 16:25, n = 3:12)

# Call the "factory-made" function:
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
