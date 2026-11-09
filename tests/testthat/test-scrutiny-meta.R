test_that("`scrutiny_meta()` returns the recorded settings, or `NULL`", {
  out <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "up")
  out |> scrutiny_meta() |> expect_equal(tibble::tibble(
    setting = c("x_meaning", "rounding", "threshold", "symmetric", "show_rec"),
    value = c("'mean'", "'up'", "5", "FALSE", "FALSE"),
    default = c(TRUE, FALSE, TRUE, TRUE, TRUE),
    added_by = "grim_map"
  ))
  out |> scrutiny_args() |> _$rounding |> expect_equal("up")
  out |> dplyr::filter(!consistency) |> scrutiny_meta() |> expect_equal(scrutiny_meta(out))
  out |> dplyr::summarise(m = mean(x)) |> scrutiny_meta() |> expect_null()
  pigs1 |> scrutiny_meta() |> expect_null()

  # Sequence and total-n mappers add their own settings, compared to their own
  # defaults; the bookkeeping for `audit_seq()` stays out of the tibble:
  out_seq <- pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 1:10)
  meta_seq <- scrutiny_meta(out_seq)
  meta_seq$added_by |> unique() |> expect_equal(c("grim_map", "grim_map_seq"))
  meta_seq |> dplyr::filter(!default) |> _$setting |> expect_equal("dispersion")
  meta_seq$setting |> intersect(c("results_reported", "name_key_result")) |> expect_length(0L)
  out_seq |> attr("scrutiny") |> _$results_reported |> expect_equal(out$consistency)

  out_total_n <- tibble::tibble(x1 = 4.71, x2 = 5.3, n = 40) |> grim_map_total_n(digits_x = c(2, 1), items_averaged = 1, data_type = "integer", n_min = 5)
  out_total_n |> scrutiny_meta() |> dplyr::filter(!default) |> _$value |> expect_equal("5")
  out_total_n |> scrutiny_meta() |> _$added_by |> unique() |> expect_equal(c("grim_map", "grim_map_total_n"))

  # An enum is compared to its first value, and a value equal to the default
  # counts as default whatever its type:
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", sd_meaning = "sd_sample") |> scrutiny_meta() |> _$default |> all() |> expect_true()
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", threshold = 5L) |> scrutiny_meta() |> _$default |> all() |> expect_true()
})


test_that("values print as code, in single quotes where that is safe", {
  c('"up"', 'c("x", "n")', "1:5", '"it\'s"') |> single_quotes() |> expect_equal(c("'up'", "c('x', 'n')", "1:5", '"it\'s"'))
})


test_that("the print header names the settings that differ from the defaults", {
  header <- function(x) pillar::tbl_sum(x)[-1L]
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "up") |> header() |> expect_equal('GRIM: rounding = "up"')
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", threshold = 5L)  |> header() |> expect_equal("GRIM: default settings")
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", sd_meaning = "sd_sample_or_pop") |> header() |> expect_equal('GRIMMER: sd_meaning = "sd_sample_or_pop"')
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", sd_meaning = "se")        |> header() |> expect_equal('GRIMMER: sd_meaning = "se"')
  pigs3 |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer", show_rec = TRUE) |> header() |> expect_equal("DEBIT: show_rec = TRUE")
  # Sequence output gets the basic mapper's line; a dropped attribute drops it:
  pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer", symmetric = TRUE, dispersion = 1:3) |> header() |> expect_equal("GRIM: symmetric = TRUE, dispersion = 1:3")
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> structure(scrutiny = NULL) |> header() |> expect_length(0L)
  # The first line is unchanged, but unnamed, so that pillar adds no padding:
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> pillar::tbl_sum() |> _[[1L]] |> expect_match("^A tibble: 12 . 7$")
})


test_that("`scrutiny_args()` returns no tier settings if the basic mapper recorded none", {
  # Only the sequence mapper's rows, as from a basic mapper without settings:
  out <- tibble::tibble(x = 1)
  attr(out, "scrutiny") <- list(settings = new_settings(list(dispersion = 1:5), list(), "my_test_map_seq"))
  out |> scrutiny_args() |> expect_length(0L)
  pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer") |> scrutiny_args() |> rlang::has_name("dispersion") |> expect_false()
})


test_that("a setting close to its default is not recorded as the default", {
  # `all.equal()` has a tolerance, which is absolute near zero:
  sum_check <- function(parts, total, tolerance = 0) abs(sum(parts) - total) <= tolerance
  sum_check_map <- function_map(sum_check, .reported = "total", .reported_variadic = "parts", .name_test = "SUMCHECK")
  d <- tibble::tibble(a = 0.1, b = 0.2, total = 0.3)
  default_of <- function(out) scrutiny_meta(out)$default
  d |> sum_check_map(parts = c(a, b))                   |> default_of() |> expect_true()
  d |> sum_check_map(parts = c(a, b), tolerance = 0L)   |> default_of() |> expect_true()
  d |> sum_check_map(parts = c(a, b), tolerance = 1e-9) |> default_of() |> expect_false()
})


test_that("`scrutiny_meta()` takes `data`", {
  out <- tibble::tibble(x = 5.19, n = 28) |> grim_map(2, items_averaged = 1, data_type = "integer")
  scrutiny_meta(data = out) |> expect_equal(scrutiny_meta(out))
})
