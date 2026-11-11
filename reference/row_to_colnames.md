# Turn row values into column names

Tables extracted from PDFs, e.g. with tabulapdf, sometimes have their
column names in the first row (or rows) and nondescript names above.
`row_to_colnames()` replaces the column names by the values of those
rows and drops the rows.

## Usage

``` r
row_to_colnames(data, row = 1L, collapse = " ", drop = TRUE)
```

## Arguments

- data:

  Data frame or matrix.

- row:

  Integer. Position of the row or rows that contain the column names.
  Default is `1`.

- collapse:

  String. Separator between the values of several rows. Default is
  `" "`.

- drop:

  Logical. Should the rows be removed? Default is `TRUE`.

## Value

`data` with new column names, as a data frame of the same class, or a
tibble if `data` was a matrix.

## Details

With several rows, their values are pasted together per column. Missing
values, blank strings, `"NA"`, and `"NaN"` are skipped, and whitespace
around a value is trimmed; if all of a column's values in these rows are
missing or blank, it keeps its name. The new names must be unique: a
header that repeats, as in "M, SD, M, SD", is an error unless a second
row tells the columns apart. A label that spans several columns, such as
a group name above "M, SD", is often only in the first of them; repeat
it in each one first.

## See also

`unheadr::mash_colnames()`, a more general solution to the same problem.
