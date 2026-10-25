test_that("`scrutiny_meta()` returns the recorded settings, or `NULL`", {
  out <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, rounding = "up")
  out |> scrutiny_meta() |> _$args$rounding |> expect_equal("up")
  out |> scrutiny_meta() |> _$args$percent  |> expect_false()
  out |> dplyr::filter(!consistency) |> scrutiny_meta() |> expect_equal(scrutiny_meta(out))
  out |> dplyr::summarise(m = mean(x)) |> scrutiny_meta() |> expect_null()
  pigs1 |> scrutiny_meta() |> expect_null()

  out_seq <- pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1)
  out_seq |> scrutiny_meta() |> names() |>
    expect_setequal(c("args", "results_reported", "name_key_result", "var", "dispersion_linear"))
  out_seq |> scrutiny_meta() |> _$results_reported |> expect_equal(out$consistency)
})


test_that("the print header names the settings that differ from the defaults", {
  header <- function(x) pillar::tbl_sum(x)[-1L]
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, rounding = "up") |> header() |> expect_equal('GRIM: rounding = "up"')
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, threshold = 5L)  |> header() |> expect_equal("GRIM: default settings")
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, sd_type = "either") |> header() |> expect_equal('GRIMMER: sd_type = "either"')
  pigs3 |> debit_map(digits_x = 2, digits_sd = 2, show_rec = TRUE) |> header() |> expect_equal("DEBIT: show_rec = TRUE")
  # Sequence output gets the basic mapper's line; a dropped attribute drops it:
  pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1, symmetric = TRUE) |> header() |> expect_equal("GRIM: symmetric = TRUE")
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1) |> structure(scrutiny = NULL) |> header() |> expect_length(0L)
  # The first line is unchanged, but unnamed, so that pillar adds no padding:
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1) |> pillar::tbl_sum() |> _[[1L]] |> expect_match("^A tibble: 12 . 6$")
})
