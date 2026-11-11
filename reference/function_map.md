# Create new `*_map()` functions

`function_map()` creates a basic mapper: a function that applies a
single-case test such as `grim_scalar()` to every row of a data frame.
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md),
and
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
are made by it.

See
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md),
section *Creating basic mappers with `function_map()`*.

`function_map()` is a developer tool: it may change in a minor release,
with a NEWS entry.

## Usage

``` r
function_map(
  .fn,
  .reported = NULL,
  .name_test,
  .name_key_result = "consistency",
  .name_class = NULL,
  .args_disabled = NULL,
  .args_by_row = NULL,
  .args_fixed = NULL,
  .cols_helper = NULL,
  .col_names = NULL,
  .cols_derived = NULL,
  .reported_variadic = NULL,
  ...
)
```

## Arguments

- .fn:

  Single-case function to apply to each row. It must return a single
  logical value or, if `.col_names` is specified, a list of values with
  the key result first.

- .reported:

  String. Names of the arguments of `.fn` that take the reported values,
  such as `c("x", "n")`. These become the key columns. May be `NULL` if
  `.reported_variadic` is specified. Key and helper columns named `n`,
  `items_averaged`, or `absolute` are taken to be whole numbers, as are
  the decimal counts of `.args_by_row`: a value within floating-point
  noise of one, such as `100 * 0.29`, is rounded to it before `.fn` sees
  it.

- .name_test:

  String (length 1). Plain-text name of the test, such as `"GRIM"`.

- .name_key_result:

  (Experimental) String (length 1). Name of the key result column.
  Default is `"consistency"`.

- .name_class:

  String. Optionally, one or more classes to add to the output. Default
  is `NULL`; see *Details* for the class that is always added.

- .args_disabled:

  Optionally, names of arguments of `.fn` that the factory-made function
  should not have. Specifying one of them is an error.

- .args_by_row:

  Optionally, names of arguments of `.fn` that may vary by row, such as
  `"digits_x"`. The factory-made function takes a single value or one
  per row of `data` for each. If one is left out, it is read from a
  column of `data` by that name, if there is one. They are returned as
  columns. One whose name starts with `digits_` is a decimal count,
  returned as integer like `digits_x`.

- .args_fixed:

  Optionally, a named list of values that are always passed to `.fn`,
  such as `list(show_reason = TRUE)`. The factory-made function has no
  arguments for them; specifying one is an error.

- .cols_helper:

  Optionally, names of arguments of `.fn` that may also be given as
  columns of `data`, such as `"items_averaged"`. A helper column is
  returned after the key columns if it was given, as a column or as an
  argument. Giving both is an error if they disagree.

- .col_names:

  Optionally, names of the columns that a list returned by `.fn` unpacks
  into, as with `show_rec = TRUE`. The first name must be
  `.name_key_result`. If `data` has no rows, the columns are those that
  `.fn` returns for a row of missing values, or, if it fails on one,
  only the key result column.

- .cols_derived:

  Optionally, a named list of vectorized functions that compute further
  columns from the same input, such as
  `list(exp_incon = grim_exp_incon)`. Each is called once, with the
  whole columns and only the arguments it has formals for, and must
  return one value per row; a `.reported_variadic` argument comes as a
  list of each row's values. The columns follow the key result column.

- .reported_variadic:

  Optionally, a single string naming an argument of `.fn` that takes the
  values of one row as a vector. The factory-made function gets an
  argument by that name, without a default, that selects any number of
  columns with tidyselect syntax. See *Details*.

- ...:

  These dots must be empty.

## Value

A function with these arguments:

- `data`: a data frame with the key columns. Other columns are
  permitted.

- The `.args_by_row` arguments, such as `digits_x`.

- If `.reported_variadic` was given, an argument by that name.

- One argument per `.reported` value, for naming a column of `data` that
  holds that value under another name.

- `...`: must be empty. Every argument after it must be named in full.

- The other arguments of `.fn`, i.e., those not named in `.reported`,
  `.reported_variadic`, `.args_disabled`, or `.args_fixed`, with the
  same defaults.

## Details

`.reported_variadic` is for a test whose number of key columns depends
on the data, such as whether the values in any number of columns add up
to the value in another one. `.fn` takes those values as a single
vector, and the factory-made function selects the columns with
tidyselect; see *Examples*. The selection must not be empty and must not
include a column that already has a role in the test. The selected
columns are returned as themselves. Such a mapper cannot be wrapped by
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
or
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md),
which derive their arguments from `.reported`.

The output has the class `"scrutiny_{tolower(.name_test)}_map"`, with
spaces as underscores, e.g., `"scrutiny_grim_map"`, in addition to any
`.name_class`. The arguments that applied to the whole call, such as
`rounding`, are recorded on the output; see
[`scrutiny_meta()`](https://lhdjung.github.io/scrutiny/reference/scrutiny_meta.md).

## Value returned by the factory-made function

A tibble with the `.reported_variadic` columns, the `.reported` columns,
the helper columns that were given, the `.args_by_row` columns, and the
key result column (`consistency` by default), which is `TRUE` for
consistent value sets, `FALSE` for inconsistent ones, and `NA` if the
test can't decide. Any `.cols_derived` columns, any `.col_names`
columns, and the other columns of `data` follow, in that order.

## Examples

``` r
# A single-case function for SCHLIM, a mock test:
schlim_scalar <- function(y, n) {
  (y / 3) > n
}

# The mapper:
schlim_map <- function_map(
  .fn = schlim_scalar,
  .reported = c("y", "n"),
  .name_test = "SCHLIM"
)

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

# A test over any number of columns: do the parts add up to the total?
# The single-case function takes the parts as one vector...
sum_check_scalar <- function(parts, total, tolerance = 0) {
  abs(sum(parts) - total) <= tolerance
}

# ...which `.reported_variadic` names:
sum_check_map <- function_map(
  .fn = sum_check_scalar,
  .reported = "total",
  .reported_variadic = "parts",
  .name_test = "SUMCHECK"
)

df2 <- tibble::tibble(
  item_1 = c(10, 20, 30),
  item_2 = c(5, 5, 5),
  item_3 = c(1, 2, 3),
  total  = c(16, 27, 40)
)

# The `parts` argument selects columns with tidyselect syntax:
sum_check_map(df2, parts = starts_with("item"))
#> # A tibble: 3 × 5
#>   item_1 item_2 item_3 total consistency
#>    <dbl>  <dbl>  <dbl> <dbl> <lgl>      
#> 1     10      5      1    16 TRUE       
#> 2     20      5      2    27 TRUE       
#> 3     30      5      3    40 FALSE      
```
