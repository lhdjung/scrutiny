# Example data ------------------------------------------------------------

pigs5_renamed <- pigs5 |>
  dplyr::rename(q = x, w = sd, e = n)


# Expected output ---------------------------------------------------------

pigs5_exp <- tibble::tibble(
  x = c(7.22, 4.74, 5.23, 2.57, 6.77, 2.68, 7.01, 7.38, 3.14, 6.89, 5, 0.24),
  sd = c(5.3, 6.55, 2.55, 2.57, 2.18, 2.59, 6.68, 3.65, 5.32, 4.18, 2.18, 6.43),
  n = c(38, 31, 35, 30, 33, 34, 35, 32, 33, 37, 31, 34),
  digits_x = rep(2, 12L),
  digits_sd = rep(2, 12L),
  consistency = c(
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    TRUE
  ),
  reason = c(
    "GRIM inconsistent",
    "Passed all",
    "GRIMMER inconsistent (test 3)",
    "Passed all",
    "GRIM inconsistent",
    "Passed all",
    "GRIM inconsistent",
    "Passed all",
    "GRIM inconsistent",
    "Passed all",
    "Passed all",
    "Passed all"
  ),
) |>
  structure(
    class = c(
      "scrutiny_grimmer_map",
      "scrutiny_rounding_up_or_down",
      "tbl_df",
      "tbl",
      "data.frame"
    )
  )


# Testing -----------------------------------------------------------------

test_that("`grimmer_map()` works correctly by default", {
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2) |> expect_equal(pigs5_exp)
})

test_that("`grimmer_map()` works correctly with columns renamed", {
  pigs5_renamed |> grimmer_map(digits_x = 2, digits_sd = 2, x = q, sd = w, n = e) |>
    expect_equal(pigs5_exp)
})

test_that("`grimmer_map()` returns other columns from `data`", {
  pigs5_extra <- pigs5 |>
    dplyr::mutate(study = seq_len(nrow(pigs5)), note = "hi")
  out <- pigs5_extra |> grimmer_map(digits_x = 2, digits_sd = 2)
  # The extra columns come along, to the right of the key result columns...
  out |> colnames() |> expect_equal(c(colnames(pigs5_exp), "study", "note"))
  out$study |> expect_equal(pigs5_extra$study)
  out$note  |> expect_equal(pigs5_extra$note)
  # ...and the test results themselves are unaffected:
  out[colnames(pigs5_exp)] |> expect_equal(pigs5_exp)
})

test_that("`grimmer_map()` doesn't duplicate `digits_*` columns from `data`", {
  # `function_map_seq_proto()` hands the mapper every column to the left of
  # `"consistency"`, which includes `digits_x` and `digits_sd`. A second copy
  # would be name-repaired and then forwarded back as a bogus argument.
  pigs5[1:3, ] |>
    dplyr::mutate(digits_x = 2, digits_sd = 2) |>
    grimmer_map(digits_x = 2, digits_sd = 2) |>
    colnames() |>
    expect_equal(c(
      "x", "sd", "n", "digits_x", "digits_sd", "consistency", "reason"
    ))
})

test_that("`grimmer_map()` returns other columns with `show_reason = FALSE`", {
  pigs5 |>
    dplyr::mutate(study = seq_len(nrow(pigs5))) |>
    grimmer_map(digits_x = 2, digits_sd = 2, show_reason = FALSE) |>
    colnames() |>
    expect_equal(c(
      "x", "sd", "n", "digits_x", "digits_sd", "consistency", "study"
    ))
})


# GRIMMER's SD can be 0, and the sequence mapper used to put that value out of
# reach: `out_min = "auto"` stopped one decimal unit above zero.

test_that("`grimmer_map_seq()` can disperse an SD down to zero, but no lower", {
  out <- grimmer_map_seq(
    tibble::tibble(x = 1.03, sd = 0.03, n = 40),
    digits_x = 2,
    digits_sd = 2,
    var = "sd",
    include_consistent = TRUE
  )
  min(out$sd) |> expect_equal(0)
  any(out$sd < 0) |> expect_false()
})
