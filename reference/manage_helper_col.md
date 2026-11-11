# Manage a helper column

**\[superseded\]**

`manage_helper_col()` is superseded by the `.cols_helper` argument of
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md).

A helper column is a column of a mapper's input that is not itself
tested but enters the test, such as `items_averaged` in
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
It can be given either as a column of `data` or as an argument of the
same name. `manage_helper_col()` checks that the two don't disagree if
both are given and, by default, adds the argument's value to `data` as a
column if `data` has none.

Mappers made by
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
call it for every `.cols_helper` argument. Call it yourself in a
handwritten mapper, once per helper column, and reassign the result to
`data`.

## Usage

``` r
manage_helper_col(
  data,
  arg,
  default,
  add_col = TRUE,
  var_arg = deprecated(),
  affix = deprecated()
)
```

## Arguments

- data:

  The mapper's input data frame.

- arg:

  The mapper argument named like the helper column.

- default:

  The default of that argument. Leave it out if the argument has none,
  like `items_averaged`: `data` is then returned unchanged if the
  argument is missing, and the test function reports that.

- add_col:

  Logical. If `data` has no such column, should one be added from `arg`?
  Default is `TRUE`.

- var_arg, affix:

  **\[deprecated\]** The old names of `arg` and `add_col`.

## Value

`data`, possibly with the helper column added.
