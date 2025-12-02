# Documentation template for function factory conventions

`write_doc_factory_map_conventions()` creates a roxygen2 block section
to be inserted into the documentation of a function factory such as
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
or
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md).
It lays out the naming guidelines that users of your function factory
should follow when creating new manufactured functions.

Copy the output from your console and paste it into the roxygen2 block
of your function factory.

## Usage

``` r
write_doc_factory_map_conventions(
  ending,
  name_test1 = "GRIM",
  name_test2 = "GRIMMER",
  scrutiny_prefix = FALSE
)
```

## Arguments

- ending:

  String (length 1). The part of your function factory's name after
  `function_map_`. To

- name_test1, name_test2:

  Strings (length 1 each). Plain-text names of example consistency
  tests. Defaults are `"GRIM"` and `"GRIMMER"`, respectively.

- scrutiny_prefix:

  Logical (length 1). Should the scrutiny functions mentioned in the
  output have a `scrutiny::` namespace specification? Set this to `TRUE`
  if the output will go into another package's documentation. Default is
  `FALSE`.

## Value

A string vector formatted by
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

## See also

For context, see [*Implementing consistency
tests*](https://lhdjung.github.io/scrutiny/articles/consistency-tests.html).

## Examples

``` r
# For `function_map_seq()`:
write_doc_factory_map_conventions(ending = "seq")
#> #' @section Conventions: The name of a function returned by 
#> #'   `function_map_seq()` should mechanically follow from that of 
#> #'   the input function. For example, [`grim_map_seq()`] derives 
#> #'   from [`grim_map()`]. This pattern fits best if the input function itself 
#> #'   is named after the test it performs on a data frame, followed by `_map`: 
#> #'   [`grim_map()`] applies GRIM, [`grimmer_map()`] applies GRIMMER, etc. 
#> #' 
#> #'   Much the same is true for the classes of data frames returned by the 
#> #'   manufactured function via the `.name_class` argument of 
#> #'   `function_map_seq()`. It should be the function's own name preceded 
#> #'   by the name of the package that contains it, or by an acronym of that 
#> #'   package's name. Therefore, some existing classes are 
#> #'   `scr_grim_map_seq` and `scr_grimmer_map_seq`. 

# For `function_map_total_n()`:
write_doc_factory_map_conventions(ending = "total_n")
#> #' @section Conventions: The name of a function returned by 
#> #'   `function_map_total_n()` should mechanically follow from that of 
#> #'   the input function. For example, [`grim_map_total_n()`] derives 
#> #'   from [`grim_map()`]. This pattern fits best if the input function itself 
#> #'   is named after the test it performs on a data frame, followed by `_map`: 
#> #'   [`grim_map()`] applies GRIM, [`grimmer_map()`] applies GRIMMER, etc. 
#> #' 
#> #'   Much the same is true for the classes of data frames returned by the 
#> #'   manufactured function via the `.name_class` argument of 
#> #'   `function_map_total_n()`. It should be the function's own name preceded 
#> #'   by the name of the package that contains it, or by an acronym of that 
#> #'   package's name. Therefore, some existing classes are 
#> #'   `scr_grim_map_total_n` and `scr_grimmer_map_total_n`. 
```
