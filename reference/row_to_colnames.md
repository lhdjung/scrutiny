# Turn row values into column names

Data frames sometimes have wrong column names, while the correct column
names are stored in one or more rows in the data frame itself. To remedy
this issue, call `row_to_colnames()` on the data frame: It replaces the
column names by the values of the specified rows (by default, only the
first one). These rows are then dropped by default.

## Usage

``` r
row_to_colnames(data, row = 1L, collapse = " ", drop = TRUE)
```

## Arguments

- data:

  Data frame or matrix.

- row:

  Integer. Position of the rows (one or more) that jointly contain the
  correct column names. Default is `1`.

- collapse:

  String. If the length of `row` is greater than 1, each new column name
  will be that many row values pasted together. `collapse`, then, is the
  substring between two former row values in the final column names.
  Default is `" "` (a space).

- drop:

  Logical. If `TRUE` (the default), the rows specified with `row` are
  removed.

## Value

A tibble (data frame).

## Details

If multiple rows are specified, the row values for each individual
column are pasted together. Some special characters might then be
missing.

This function might be useful when importing tables from PDF, e.g. with
[tabulizer](https://cran.r-project.org/package=tabulizer). In R, these
data frames (converted from matrices) do sometimes have the issue
described above.

## See also

`unheadr::mash_colnames()`, a more sophisticated solution to the same
problem.
