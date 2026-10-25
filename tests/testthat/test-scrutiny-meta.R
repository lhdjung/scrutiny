test_that("`scrutiny_meta()` returns the recorded settings, or `NULL`", {
  out <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, rounding = "up")
  out |> scrutiny_meta() |> expect_equal(tibble::tibble(
    setting = c("percent", "show_rec", "rounding", "threshold", "symmetric"),
    value = c("FALSE", "FALSE", "'up'", "5", "FALSE"),
    default = c(TRUE, TRUE, FALSE, TRUE, TRUE),
    added_by = "grim_map"
  ))
  out |> scrutiny_args() |> _$rounding |> expect_equal("up")
  out |> dplyr::filter(!consistency) |> scrutiny_meta() |> expect_equal(scrutiny_meta(out))
  out |> dplyr::summarise(m = mean(x)) |> scrutiny_meta() |> expect_null()
  pigs1 |> scrutiny_meta() |> expect_null()

  # Sequence and total-n mappers add their own settings, compared to their own
  # defaults; the bookkeeping for `audit_seq()` stays out of the tibble:
  out_seq <- pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1, dispersion = 1:10)
  meta_seq <- scrutiny_meta(out_seq)
  meta_seq$added_by |> unique() |> expect_equal(c("grim_map", "grim_map_seq"))
  meta_seq |> dplyr::filter(!default) |> _$setting |> expect_equal("dispersion")
  meta_seq$setting |> intersect(c("results_reported", "name_key_result", "dispersion_linear")) |> expect_length(0L)
  out_seq |> attr("scrutiny") |> _$results_reported |> expect_equal(out$consistency)

  out_total_n <- tibble::tibble(x1 = 4.71, x2 = 5.3, n = 40) |> grim_map_total_n(digits_x = c(2, 1), items_averaged = 1, n_min = 5)
  out_total_n |> scrutiny_meta() |> dplyr::filter(!default) |> _$value |> expect_equal("5")
  out_total_n |> scrutiny_meta() |> _$added_by |> unique() |> expect_equal(c("grim_map", "grim_map_total_n"))

  # An enum is compared to its first value, and a value equal to the default
  # counts as default whatever its type:
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, sd_type = "sample") |> scrutiny_meta() |> _$default |> all() |> expect_true()
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, threshold = 5L) |> scrutiny_meta() |> _$default |> all() |> expect_true()
})


test_that("values print as code, in single quotes where that is safe", {
  c('"up"', 'c("x", "n")', "1:5", '"it\'s"') |> single_quotes() |> expect_equal(c("'up'", "c('x', 'n')", "1:5", '"it\'s"'))
})


test_that("the print header names the settings that differ from the defaults", {
  header <- function(x) pillar::tbl_sum(x)[-1L]
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, rounding = "up") |> header() |> expect_equal('GRIM: rounding = "up"')
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, threshold = 5L)  |> header() |> expect_equal("GRIM: default settings")
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, sd_type = "either") |> header() |> expect_equal('GRIMMER: sd_type = "either"')
  pigs3 |> debit_map(digits_x = 2, digits_sd = 2, show_rec = TRUE) |> header() |> expect_equal("DEBIT: show_rec = TRUE")
  # Sequence output gets the basic mapper's line; a dropped attribute drops it:
  pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1, symmetric = TRUE, dispersion = 1:3) |> header() |> expect_equal("GRIM: symmetric = TRUE, dispersion = 1:3")
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1) |> structure(scrutiny = NULL) |> header() |> expect_length(0L)
  # The first line is unchanged, but unnamed, so that pillar adds no padding:
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1) |> pillar::tbl_sum() |> _[[1L]] |> expect_match("^A tibble: 12 . 6$")
})
