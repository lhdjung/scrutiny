# Read the settings a mapper recorded on its output

**\[experimental\]**

Every mapper, such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
records the settings of the call that produced its output.
`scrutiny_meta()` returns them as a tibble, so that a report can state
the rounding method or SD convention behind a result.

Settings are the arguments that apply to the whole call, with defaults
resolved, such as `rounding`, `x_meaning`, or `sd_meaning`. Arguments
that may vary by row, such as `digits_x` and `items_averaged`, are
columns of the output instead, even if given as a single value. Sequence
and total-n mappers, such as
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md),
add their own settings, such as `dispersion`.

Printed output of
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md),
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md),
or
[`percentage_of_map()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map.md),
including sequence and total-n output, has a header line that names the
settings that are not at their defaults, as in
`# GRIM: rounding = "up"`, or says `default settings`.

Some dplyr verbs, such as `summarise()`, drop the record, and with it
the header line; [`filter()`](https://rdrr.io/r/stats/filter.html),
`arrange()`, and `mutate()` keep it.

## Usage

``` r
scrutiny_meta(data)
```

## Arguments

- data:

  Output of a mapper function.

## Value

A tibble with one row per setting and these columns, or `NULL` if `data`
has no record of its settings:

- `setting`: the name of the argument.

- `value`: its value, written as R code, such as `'up'` or `1:5`.

- `default`: `TRUE` if the value is the mapper's default, `FALSE` if
  not.

- `added_by`: the mapper whose argument it is, such as `"grim_map"` or
  `"grim_map_seq"`.

## Examples

``` r
pigs1 |>
  grim_map(
    digits_x = 2,
    items_averaged = 1,
    data_type = "integer",
    rounding = "up"
  ) |>
  scrutiny_meta()
#> # A tibble: 5 × 4
#>   setting   value  default added_by
#>   <chr>     <chr>  <lgl>   <chr>   
#> 1 x_meaning 'mean' TRUE    grim_map
#> 2 rounding  'up'   FALSE   grim_map
#> 3 threshold 5      TRUE    grim_map
#> 4 symmetric FALSE  TRUE    grim_map
#> 5 show_rec  FALSE  TRUE    grim_map

# Only the settings that are not at their defaults:
pigs1 |>
  grim_map_seq(
    digits_x = 2,
    items_averaged = 1,
    data_type = "integer",
    dispersion = 1:10
  ) |>
  scrutiny_meta() |>
  dplyr::filter(!default)
#> # A tibble: 1 × 4
#>   setting    value default added_by    
#>   <chr>      <chr> <lgl>   <chr>       
#> 1 dispersion 1:10  FALSE   grim_map_seq
```
