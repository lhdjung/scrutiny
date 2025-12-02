# Check that a mapper's input has correct column names

When called within a consistency test mapper function,
`check_mapper_input_colnames()` makes sure that the input data frame has
correct column names:

- They include all the key columns corresponding to the test applied by
  the mapper.

- They don't already include `"consistency"`.

If either check fails, the function throws an informative error.

## Usage

``` r
check_mapper_input_colnames(data, reported, name_test)
```

## Arguments

- data:

  Data frame. Input to the mapper function.

- reported:

  String vector of the "key" column names that `data` must have, such as
  `c("x", "n")` for
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).

- name_test:

  String (length 1). Short, plain-text name of the consistency test that
  the mapper function applies, such as `"GRIM"`.

## Value

No return value. Might throw an error.

## See also

[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md),
for context and the "key columns" terminology.
