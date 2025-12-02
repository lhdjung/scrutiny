# General interface to reconstructing rounded numbers

`reround()` takes one or more intermediate reconstructed values and
rounds them in some specific way – namely, the way they are supposed to
have been rounded originally, in the process that generated the reported
values.

This function provides an interface to all of scrutiny's rounding
functions as well as
[`base::round()`](https://rdrr.io/r/base/Round.html). It is used as a
helper within
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
[`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
and [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md);
and it might find use in other places for consistency testing or
reconstruction of statistical analyses.

## Usage

``` r
reround(
  x,
  digits = 0L,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
```

## Arguments

- x:

  Numeric. Vector of possibly original values.

- digits:

  Integer. Number of decimal places in the reported key values (i.e.,
  mean or percentage within
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md), or
  standard deviation within
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md)).

- rounding:

  String. The rounding method that is supposed to have been used
  originally. See
  [`vignette("rounding-options")`](https://lhdjung.github.io/scrutiny/articles/rounding-options.md).
  Default is `"up_or_down"`, which returns two values: `x` rounded up
  *and* down.

- threshold:

  Integer. If `rounding` is set to `"up_from"`, `"down_from"`, or
  `"up_from_or_down_from"`, `threshold` must be set to the number from
  which the reconstructed values should then be rounded up or down.
  Otherwise irrelevant. Default is `5`.

- symmetric:

  Logical. Set `symmetric` to `TRUE` if the rounding of negative numbers
  with `"up_or_down"`, `"up"`, `"down"`, `"up_from_or_down_from"`,
  `"up_from"`, or `"down_from"` should mirror that of positive numbers
  so that their absolute values are always equal. Otherwise irrelevant.
  Default is `FALSE`.

## Value

Numeric vector of length 1 or 2. (It has length 1 unless `rounding` is
`"up_or_down"`, `"up_from_or_down_from"`, or`"ceiling_or_floor"`, in
which case it has length 2.)

## Details

`reround()` internally calls the appropriate rounding function(s)
determined by the `rounding` argument. See
[`vignette("rounding-options")`](https://lhdjung.github.io/scrutiny/articles/rounding-options.md)
for a complete list of values that `rounding` can take.

For the specific rounding functions themselves, see documentation at
[`round_up()`](https://lhdjung.github.io/scrutiny/reference/rounding-common.md),
[`round_ceiling()`](https://lhdjung.github.io/scrutiny/reference/rounding-uncommon.md),
and [`base::round()`](https://rdrr.io/r/base/Round.html).
