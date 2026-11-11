# Create new `*_map_total_n()` functions

`function_map_total_n()` creates a total-n mapper from a basic mapper
such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md):
a function for two groups whose statistics are reported with the total
sample size but without the group sizes, as
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
is. It uses
[`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
to create pairs of hypothetical group sizes that add up to the total,
and tests the reported statistics against each pair.

See
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md),
section *Total-n mapper*.

`function_map_total_n()` is a developer tool: it may change in a minor
release, with a NEWS entry.

## Usage

``` r
function_map_total_n(
  .fn,
  .reported,
  .name_test,
  .name_key_result = "consistency",
  .name_class = NULL,
  .dispersion = 0:5,
  .n_min = 1L,
  .n_max = NULL,
  .constant = NULL,
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

- .reported:

  String. Names of the group-specific statistics reported alongside the
  total, such as `"x"` for GRIM and `c("x", "sd")` for DEBIT. In the
  input of the factory-made function, each appears as two columns with
  the suffixes `1` and `2`, such as `x1` and `x2`.

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

- .dispersion, .n_min, .n_max, .constant:

  Passed on to
  [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md),
  with the defaults from there. `.n_min` may also be a function of the
  settings of `.fn`, as a named list, that returns the default `n_min`
  for a call; the factory-made function's `n_min` then defaults to
  `NULL`, which stands for that value.

- ...:

  These dots must be empty.

## Value

A function such as these:

|  |  |  |
|----|----|----|
| **Factory-made function** | **Reported statistics** | **Test vignette** |
| [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md) | `"x"` | [`vignette("grim-in-depth")`](https://lhdjung.github.io/scrutiny/articles/grim-in-depth.md) |
| [`grimmer_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_total_n.md) | `"x"`, `"sd"` | [`vignette("grimmer-in-depth")`](https://lhdjung.github.io/scrutiny/articles/grimmer-in-depth.md) |
| [`debit_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/debit_map_total_n.md) | `"x"`, `"sd"` | [`vignette("debit")`](https://lhdjung.github.io/scrutiny/articles/debit.md) |

The settings of `.fn`, i.e., its arguments after `...`, such as
`rounding`, are arguments of this function as well, with the same
defaults. So are its helper arguments, such as `items_averaged`. Its
`...` pass any other arguments on to `.fn`, which is only of use for a
`.fn` without `...` of its own.

## Details

To export a factory-made function from another package, follow the
[purrr FAQ on
adverbs](https://purrr.tidyverse.org/reference/faq-adverbs-export.html);
see
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md),
section *Context and export*.

## Conventions

Name the factory-made function after `.fn` with `_total_n` appended, as
in
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
from
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
The output has the classes `"scrutiny_map_total_n"` and
`"scrutiny_{tolower(.name_test)}_map_total_n"`. Prefix any class you add
via `.name_class` with your package's name, as in
`"mypkg_schlim_map_total_n"`.

## References

Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It Light
or Dark? Recalling Moral Behavior Changes Perception of Brightness.
*Psychological Science*, 32(12), 2042–2043.
https://journals.sagepub.com/doi/10.1177/09567976211058727

## See also

[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)

## Examples

``` r
# Function definition of `grim_map_total_n()`:
grim_map_total_n <- function_map_total_n(
  .fn = grim_map,
  .reported = "x",
  .name_test = "GRIM"
)
```
