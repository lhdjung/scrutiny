# Create new `*_map_total_n()` functions

`function_map_total_n()` is the engine that powers functions such as
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md).
It creates new, "factory-made" functions for consistency tests such as
GRIM or GRIMMER. The new functions take reported summary statistics
(e.g., means) and apply those tests in cases where only a total sample
size is known, not group sizes.

This works by making
[`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
create multiple pairs of hypothetical group sizes, all of which add up
to the reported total. There need to be exactly two groups.

For background and more examples, see the [total-n mapper
section](https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#total-n-mapper)
of *Consistency tests in depth*.

## Usage

``` r
function_map_total_n(
  .fun,
  .reported,
  .name_test,
  .name_key_result = "consistency",
  .name_class = NULL,
  .dispersion = 0:5,
  .n_min = 1L,
  .n_max = NULL,
  .constant = NULL,
  .constant_index = NULL,
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
  `consistency` that is added to the input data frame. This modified
  data frame is then returned by `.fun`.

- .reported:

  String. Names of the columns containing group-specific statistics that
  were reported alongside the total sample size(s). They will be tested
  for consistency with the hypothetical group sizes. Examples are `"x"`
  for GRIM and `c("x", "sd")` for DEBIT. In the data frame with reported
  group statistics that the manufactured function takes as an input,
  each will need to fan out like `"x1"`, `"x2"`, `"sd1"`, and `"sd2"`.

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

- .dispersion, .n_min, .n_max, .constant, .constant_index:

  Arguments passed down to
  [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md),
  using defaults from there.

- ...:

  These dots must be empty.

## Value

A function such as these:

|                                                                                                |                         |                                                                                 |
|------------------------------------------------------------------------------------------------|-------------------------|---------------------------------------------------------------------------------|
| **Manufactured function**                                                                      | **Reported statistics** | **Test vignette**                                                               |
| [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)       | `"x"`                   | [`vignette("grim")`](https://lhdjung.github.io/scrutiny/articles/grim.md)       |
| [`grimmer_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_total_n.md) | `"x"`, `"sd"`           | [`vignette("grimmer")`](https://lhdjung.github.io/scrutiny/articles/grimmer.md) |
| [`debit_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/debit_map_total_n.md)     | `"x"`, `"sd"`           | [`vignette("debit")`](https://lhdjung.github.io/scrutiny/articles/debit.md)     |

The factory-made function will also have dots, `...`, to pass arguments
down to `.fun`, i.e., the basic mapper function.

## Details

If functions created by `function_map_total_n()` are exported from other
packages, they should be written as if they were created with [purrr
adverbs](https://purrr.tidyverse.org/reference/faq-adverbs-export.html);
see explanations there, and examples in the [export
section](https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#context-and-export)
of *Consistency tests in depth*.

This function is a so-called function factory: It produces other
functions, such as
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md).
More specifically, it is a function operator because it also takes
functions as inputs, such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
See Wickham (2019), ch. 10-11.

## Conventions

The name of a function returned by `function_map_total_n()` should
mechanically follow from that of the input function. For example,
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
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
`function_map_total_n()`. It should be the function's own name preceded
by the name of the package that contains it, or by an acronym of that
package's name. Therefore, some existing classes are
`scr_grim_map_total_n` and `scr_grimmer_map_total_n`.

## References

Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It Light
or Dark? Recalling Moral Behavior Changes Perception of Brightness.
*Psychological Science*, 32(12), 2042–2043.
https://journals.sagepub.com/doi/10.1177/09567976211058727

Wickham, H. (2019). *Advanced R* (Second Edition). CRC Press/Taylor and
Francis Group. https://adv-r.hadley.nz/index.html

## See also

[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)

## Examples

``` r
# Function definition of `grim_map_total_n()`:
grim_map_total_n <- function_map_total_n(
  .fun = grim_map,
  .reported = "x",
  .name_test = "GRIM"
)
```
