# Visualize the standard deviations that DEBIT admits

**\[experimental\]**

`debit_plot_line()` draws the DEBIT test for a single value set, as
[`grimmer_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grimmer_plot_line.md)
does for GRIMMER. Of all SDs that could have been reported with
`digits_sd` decimal places, each square is one that `n` binary values
with mean `x` could have; the others are left out. The reported SD is
marked above the row and labeled as consistent or inconsistent.

For many value sets,
[`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
shows the results of
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md).
[`debit_values()`](https://lhdjung.github.io/scrutiny/reference/debit_values.md)
lists the unrounded means and SDs behind a reported pair.

## Usage

``` r
debit_plot_line(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  zoom = c("close", "medium", "wide"),
  color_con = "#0047ff",
  color_incon = "#ff2a00",
  color_attainable = "grey75"
)
```

## Arguments

- x, sd, n, digits_x, digits_sd, sd_meaning, formula, rounding,
  threshold, symmetric:

  The same as in
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
  but `x`, `sd`, `n`, `digits_x`, and `digits_sd` must have length 1.

- data_type:

  String. `"integer"` if the values are 0 and 1, as in
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md).
  There is no default, so that every call states this assumption.
  `"continuous"` and `NA` are errors: if the data may be continuous,
  there are no values to draw.

- zoom:

  String. How much of the row to show: `"close"` (the default),
  `"medium"`, or `"wide"`; see *Details*.

- color_con, color_incon:

  Strings. Color of the marker, the strip behind the squares, and the
  square at the reported value if it is consistent. `color_con` applies
  if the value is consistent, `color_incon` if not. Defaults are
  `"#0047ff"` and `"#ff2a00"`.

- color_attainable:

  String. Color of the squares for attainable values that were not
  reported, like the consistent cells of
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)'s
  raster. Default is `"grey75"`.

## Value

A ggplot object.

## Details

`zoom` works as in
[`grim_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_line.md),
but with `zoom = "wide"`, the row runs from 0 to 1, the range of
[`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md)'s
inputs.

Under `formula = "mean_n"`, the squares are the SDs of every mean that
would be reported as `x`, whether or not `n` binary values can have it.

## Examples

``` r
# A mean of 0.35 with an SD of 0.49 is consistent with 20 binary values,
# but 0.41 is not:
debit_plot_line(
  x = 0.35,
  sd = 0.49,
  n = 20,
  digits_x = 2,
  digits_sd = 2,
  data_type = "integer"
)

debit_plot_line(
  x = 0.35,
  sd = 0.41,
  n = 20,
  digits_x = 2,
  digits_sd = 2,
  data_type = "integer"
)
```
