# Helper column operations

If your consistency test mapper function supports helper columns, call
`manage_helper_col()` internally; once for every such column. It will
check whether a helper column is compatible with its eponymous argument,
i.e., if the argument was not specified by the user but has its default
value.

By default (`affix = TRUE`), the function will add the column to the
mapper's input data frame. It returns the input data frame, so reassign
its output to that variable.

All of this only works in mapper functions that were "handwritten" using
`function()`, as opposed to those produced by
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md).
See
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md),
section *Writing mappers manually*.

## Usage

``` r
manage_helper_col(data, var_arg, default, affix = TRUE)
```

## Arguments

- data:

  The data frame that is the mapper function's first argument.

- var_arg:

  The argument to the mapper function that has the same name as the
  helper column you want to manage.

- default:

  The default for the argument that was specified in `var_arg`.

- affix:

  Logical (length 1). If `data` doesn't include the helper column
  already, should `var_arg` be added to `data`, bearing its proper name?
  Default is `TRUE`.

## Value

`data`, possibly modified (see `affix` argument).
