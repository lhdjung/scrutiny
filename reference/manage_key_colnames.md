# Enable name-independent key column identification

A handwritten mapper function for consistency tests, such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
may include arguments named after the key columns in its input data
frame. When such an argument is specified by the user as a column name
of the input data frame, it identifies a differently-named column as
that key column.

Create such functionality in three steps:

1.  Add arguments to your mapper function named after the respective key
    columns. They should be `NULL` by default; e.g.,
    `x = NULL, n = NULL`.

2.  Within the mapper, capture the user input by quoting it using
    [`rlang::enexpr()`](https://rlang.r-lib.org/reference/defusing-advanced.html).
    Reassign these values to the argument variables; e.g.,
    `x <- rlang::enexpr(x)` and `n <- rlang::enexpr(n)`.

3.  For every such argument, call `manage_key_colnames()` and reassign
    its value to the input data frame variable, adding a short
    description;
    e.g.,`data <- manage_key_colnames(data, x, "mean/proportion")` and
    `data <- manage_key_colnames(data, n, "sample size")`.

## Usage

``` r
manage_key_colnames(data, arg, description = NULL)
```

## Arguments

- data:

  The mapper function's input data frame.

- arg:

  Symbol. The quoted input variable, captured by
  [`rlang::enexpr()`](https://rlang.r-lib.org/reference/defusing-advanced.html).

- description:

  String (length 1). Short description of the column in question, to be
  inserted into an error message.

## Value

The input data frame, `data`, possibly modified.

## See also

[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md),
for context.
