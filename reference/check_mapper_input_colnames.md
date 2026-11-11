# Check that a mapper's input has the right column names

**\[superseded\]**

`check_mapper_input_colnames()` is superseded by
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
whose mappers check their input columns themselves.

Call it in a handwritten mapper. It errors if `data` lacks a key column,
or if it already has a column named like the key result column
(`"consistency"` by default).

## Usage

``` r
check_mapper_input_colnames(
  data,
  reported,
  name_test,
  name_key_result = "consistency"
)
```

## Arguments

- data:

  Data frame. Input to the mapper.

- reported:

  String. Names of the key columns that `data` must have, such as
  `c("x", "n")` for
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).

- name_test:

  String (length 1). Plain-text name of the test, such as `"GRIM"`, for
  the error message.

- name_key_result:

  String (length 1). Name of the result column that the mapper adds.
  Default is `"consistency"`.

## Value

No return value; might throw an error.

## See also

[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md).
