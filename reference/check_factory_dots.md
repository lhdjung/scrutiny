# Check that no dots-argument is misspelled

`check_factory_dots()` is called within each main function factory:
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md),
and
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md).

## Usage

``` r
check_factory_dots(fun, fun_name_scalar, ...)
```

## Arguments

- fun:

  Function applied by the function factory.

- fun_name_scalar:

  String (length 1). Name of `fun`.

- ...:

  Arguments passed by the factory-made function's user to `fun`.

## Value

No return value; might throw an error.

## Details

For the `fun_name_scalar` argument, the function requires the following
line in the entry area (i.e., the part of the function factory before
the first version of the factory-made function is created):
`fun_name <- deparse(substitute(.fun))`
