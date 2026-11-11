# Deprecated name of `grim_plot_grid()`

**\[deprecated\]**

`grim_plot()` was renamed to
[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
in scrutiny 1.0.0. It keeps the signature it had in scrutiny 0.6.1, so
that calls by position still work.

## Usage

``` r
grim_plot(
  data = NULL,
  show_data = TRUE,
  show_raster = TRUE,
  show_gradient = TRUE,
  n = NULL,
  digits = NULL,
  rounding = "up_or_down",
  color_cons = deprecated(),
  color_incons = deprecated(),
  tile_alpha = deprecated(),
  tile_size = 1.5,
  raster_alpha = deprecated(),
  raster_color = NULL,
  ...
)
```

## Arguments

- data, show_data, show_raster, show_gradient, n, rounding, tile_size:

  Passed on to
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md).

- digits, raster_color:

  The old names of
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)'s
  `digits_x` and `color_raster`.

- color_cons, color_incons, tile_alpha, raster_alpha:

  No longer have any effect. To recolor the tiles, add a color scale, as
  in the examples of
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md).

- ...:

  Other arguments of
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md),
  such as `split_by_digits`.
