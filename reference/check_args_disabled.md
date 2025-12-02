# Check that disabled arguments are not specified

If the user of the function factory specified its `.args_disabled`
argument, `check_args_disabled()` enforces this ban.

## Usage

``` r
check_args_disabled(args_disabled)
```

## Arguments

- args_disabled:

  String. One or more names of arguments of the function applied within
  the factory-made function.

## Value

No return value; might throw an error.

## Details

More precisely, it throws an error if the user of the factory-made
function specified one or more arguments which the user of the function
factory had disabled via the latter's `.args_disabled` argument. The
arguments would otherwise be passed on to the function that is mapped
within the factory-made function.

on the use of the certain arguments of the function applied by the
factory-made function.

## Examples

``` r
check_args_disabled(c("disabled1", "disabled2"))
```
