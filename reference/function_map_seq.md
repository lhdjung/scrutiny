# Create new `*_map_seq()` functions

`function_map_seq()` is the engine that powers functions such as
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md).
It creates new, "factory-made" functions that apply consistency tests
such as GRIM or GRIMMER to sequences of specified variables. The
sequences are centered around the reported values of those variables.

By default, only inconsistent values are dispersed from and tested. This
provides an easy and powerful way to assess whether small errors in
computing or reporting may be responsible for inconsistencies in
published statistics.

For background and more examples, see the [sequence mapper
section](https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#sequence-mapper)
of *Consistency tests in depth*.

## Usage

``` r
function_map_seq(
  .fun,
  .var = Inf,
  .reported,
  .name_test,
  .name_key_result = "consistency",
  .name_class = NULL,
  .args_disabled = NULL,
  .dispersion = 1:5,
  .out_min = "auto",
  .out_max = NULL,
  .include_reported = FALSE,
  .include_consistent = FALSE,
  ...
)
```

## Arguments

- .fun:

  Function such as
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
  or one made by
  [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md):
  It will be used to test columns in a data frame for consistency. Test
  results are logical and need to be contained in a column called
  `"consistency"` that is added to the input data frame. This modified
  data frame is then returned by `.fun`.

- .var:

  String. Variables that will be dispersed by the manufactured function.
  Defaults to `.reported`.

- .reported:

  String. All variables the manufactured function can disperse in
  principle.

- .name_test:

  String (length 1). The name of the consistency test, such as `"GRIM"`,
  to be optionally shown in a message when using the manufactured
  function.

- .name_key_result:

  (Experimental) Optionally, a single string that will be the name of
  the key result column in the output. Default is `"consistency"`.

- .name_class:

  String. If specified, the tibbles returned by the manufactured
  function will inherit this string as an S3 class. Default is `NULL`,
  i.e., no extra class.

- .args_disabled:

  String. Optionally, names of the basic `*_map()` function's arguments.
  These arguments will throw an error if specified when calling the
  factory-made function.

- .dispersion:

  Numeric. Sequence with steps up and down from the reported values. It
  will be adjusted to these values' decimal level. For example, with a
  reported `8.34`, the step size is `0.01`. Default is `1:5`, for five
  steps up and down.

- .out_min, .out_max:

  If specified when calling a factory-made function, output will be
  restricted so that it's not below `.out_min` or above `.out_max`.
  Defaults are `"auto"` for `.out_min`, i.e., a minimum of one decimal
  unit above zero; and `NULL` for `.out_max`, i.e., no maximum.

- .include_reported:

  Logical. Should the reported values themselves be included in the
  sequences originating from them? Default is `FALSE` because this might
  be redundant and bias the results.

- .include_consistent:

  Logical. Should the function also process consistent cases (from among
  those reported), not just inconsistent ones? Default is `FALSE`
  because the focus should be on clarifying inconsistencies.

- ...:

  These dots must be empty.

## Value

A function such as those below. ("Testable statistics" are variables
that can be selected via `var`, and are then varied. All variables
except for those in parentheses are selected by default.)

|                                                                                        |                                   |                                                                                 |
|----------------------------------------------------------------------------------------|-----------------------------------|---------------------------------------------------------------------------------|
| **Manufactured function**                                                              | **Testable statistics**           | **Test vignette**                                                               |
| [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)       | `"x"`, `"n"`, (`"items"`)         | [`vignette("grim")`](https://lhdjung.github.io/scrutiny/articles/grim.md)       |
| [`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md) | `"x"`, `"sd"`, `"n"`, (`"items"`) | [`vignette("grimmer")`](https://lhdjung.github.io/scrutiny/articles/grimmer.md) |
| [`debit_map_seq()`](https://lhdjung.github.io/scrutiny/reference/debit_map_seq.md)     | `"x"`, `"sd"`, `"n"`              | [`vignette("debit")`](https://lhdjung.github.io/scrutiny/articles/debit.md)     |

The factory-made function will also have dots, `...`, to pass arguments
down to `.fun`, i.e., the basic mapper function such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).

## Details

All arguments of `function_map_seq()` set the defaults for the arguments
in the manufactured function. They can still be specified differently
when calling the latter.

If functions created this way are exported from other packages, they
should be written as if they were created with [purrr
adverbs](https://purrr.tidyverse.org/reference/faq-adverbs-export.html);
see explanations there, and examples in the [export
section](https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#context-and-export)
of *Consistency tests in depth*.

This function is a so-called function factory: It produces other
functions, such as
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md).
More specifically, it is a function operator because it also takes
functions as inputs, such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
See Wickham (2019, ch. 10-11).

## Conventions

The name of a function returned by `function_map_seq()` should
mechanically follow from that of the input function. For example,
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
derives from
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
This pattern fits best if the input function itself is named after the
test it performs on a data frame, followed by `_map`:
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
applies GRIM,
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
applies GRIMMER, etc.

Much the same is true for the classes of data frames returned by the
manufactured function via the `.name_class` argument of
`function_map_seq()`. It should be the function's own name preceded by
the name of the package that contains it, or by an acronym of that
package's name. Therefore, some existing classes are `scr_grim_map_seq`
and `scr_grimmer_map_seq`.

## References

Wickham, H. (2019). *Advanced R* (Second Edition). CRC Press/Taylor and
Francis Group. https://adv-r.hadley.nz/index.html

## Examples

``` r
# Function definition of `grim_map_seq()`:
grim_map_seq <- function_map_seq(
  .fun = grim_map,
  .reported = c("x", "n"),
  .name_test = "GRIM",
)
```
