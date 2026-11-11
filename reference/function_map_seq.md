# Create new `*_map_seq()` functions

`function_map_seq()` creates a sequence mapper from a basic mapper such
as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md):
a function that varies the reported values a few steps up and down and
tests each resulting value set, as
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
does. By default, only inconsistent value sets are dispersed.

See
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md),
section *Sequence mapper*.

`function_map_seq()` is a developer tool: it may change in a minor
release, with a NEWS entry.

## Usage

``` r
function_map_seq(
  .fn,
  .var = .reported,
  .reported,
  .name_test,
  .name_key_result = "consistency",
  .name_class = NULL,
  .args_disabled = NULL,
  .dispersion = 1:5,
  .out_min = "auto",
  .out_max = "auto",
  .var_bounds = NULL,
  .include_reported = FALSE,
  .include_consistent = FALSE,
  ...
)
```

## Arguments

- .fn:

  A basic mapper function such as
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
  typically made by
  [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md).
  It must return its input data frame with a logical column named by
  `.name_key_result` added.

- .var:

  String. The variables that the factory-made function disperses by
  default. Defaults to `.reported`.

- .reported:

  String. All variables the factory-made function can disperse.

- .name_test:

  String (length 1). Plain-text name of the test, such as `"GRIM"`, for
  messages and the output's class.

- .name_key_result:

  (Experimental) String (length 1). Name of the key result column.
  Default is `"consistency"`. Must be the value that `.fn` was created
  with.

- .name_class:

  String. Optionally, one or more classes to add to the output. Default
  is `NULL`.

- .args_disabled:

  String. Optionally, names of arguments of `.fn` that the factory-made
  function should reject.

- .dispersion:

  Numeric. Steps up and down from the reported values, in units of the
  last decimal place. Default is `1:5`.

- .out_min, .out_max:

  Limits on the dispersed values. A number applies to every dispersed
  variable, and `NULL` removes the limit. Default is `"auto"`, which
  takes the limit from `.var_bounds`.

- .var_bounds:

  Named list, or `NULL` (the default). Each element is named after a
  variable in `.reported` and is a numeric vector of its least and
  greatest possible value, with `NA` for an unbounded side, such as
  `list(n = c(1, NA), sd = c(0, NA))`. A variable not named here is
  unbounded, except for `n`, which always has a minimum of 1. For a
  bound that depends on a setting, an element may also be a function
  that takes the settings `.fn` recorded for its call, as a named list
  such as `list(sd_meaning = "sd_sample")`, and returns such a vector.
  The bounds only keep impossible values, such as a sample size of 0,
  from being tested. They have no bearing on consistency.

- .include_reported:

  Logical. Should the reported values themselves be part of the
  sequences? Default is `FALSE`.

- .include_consistent:

  Logical. Should consistent value sets be dispersed as well? Default is
  `FALSE`.

- ...:

  These dots must be empty.

## Value

A function such as these:

|  |  |  |
|----|----|----|
| **Factory-made function** | **Dispersed variables** | **Test vignette** |
| [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md) | `"x"`, `"n"` | [`vignette("grim-in-depth")`](https://lhdjung.github.io/scrutiny/articles/grim-in-depth.md) |
| [`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md) | `"x"`, `"sd"`, `"n"` | [`vignette("grimmer-in-depth")`](https://lhdjung.github.io/scrutiny/articles/grimmer-in-depth.md) |
| [`debit_map_seq()`](https://lhdjung.github.io/scrutiny/reference/debit_map_seq.md) | `"x"`, `"sd"`, `"n"` | [`vignette("debit")`](https://lhdjung.github.io/scrutiny/articles/debit.md) |

The settings of `.fn`, i.e., its arguments after `...`, such as
`rounding`, are arguments of this function as well, with the same
defaults. So are its helper arguments, such as `items_averaged`. Its
`...` pass any other arguments on to `.fn`, which is only of use for a
`.fn` without `...` of its own.

## Details

The arguments of `function_map_seq()` set the defaults of the
corresponding arguments of the factory-made function, which can be
overridden in each call.

Each variable in `var` is dispersed around its own reported value while
the others stay as reported.

To export a factory-made function from another package, follow the
[purrr FAQ on
adverbs](https://purrr.tidyverse.org/reference/faq-adverbs-export.html);
see
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md),
section *Context and export*.

## Conventions

Name the factory-made function after `.fn` with `_seq` appended, as in
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
from
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
The output has the classes `"scrutiny_map_seq"` and
`"scrutiny_{tolower(.name_test)}_map_seq"`. Prefix any class you add via
`.name_class` with your package's name, as in `"mypkg_schlim_map_seq"`.

## Examples

``` r
# Function definition of `grim_map_seq()`:
grim_map_seq <- function_map_seq(
  .fn = grim_map,
  .reported = c("x", "n"),
  .name_test = "GRIM"
)
```
