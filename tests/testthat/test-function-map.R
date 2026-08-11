# Manufactured functions --------------------------------------------------

# Stripped-down versions of the real mappers: same `.fun` and `.reported`, but
# none of the arguments that give the real ones their extra columns. They exist
# to check that the factory's core -- key columns, renaming, the key result
# column, the rounding class -- is what the real mappers get from it, and that
# the extras really are extras.

grim_map_alt <- function_map(
  .fun = grim_scalar,
  .reported = c("x", "n"),
  .name_test = "GRIM"
)

debit_map_alt <- function_map(
  .reported = c("x", "sd", "n"),
  .fun = debit_scalar,
  .name_test = "DEBIT"
)

grim_map_alt_renamed <- function_map(
  .fun = grim_scalar,
  .reported = c("x", "n"),
  .name_test = "GRIM",
  .name_key_result = "success"
)

debit_map_alt_renamed <- function_map(
  .reported = c("x", "sd", "n"),
  .fun = debit_scalar,
  .name_test = "DEBIT",
  .name_key_result = "success"
)


# Example data ------------------------------------------------------------

df_grim1 <- pigs1
df_debit1 <- pigs3

# Create this many random numbers per column:
n_dfs2 <- 150

df_grim2 <- tibble::tibble(
  x = runif(n_dfs2, 0, 10) |> round(2),
  n = runif(n_dfs2, 40, 100) |> round(0)
)

df_debit2 <- tibble::tibble(
  x = runif(n_dfs2, 0.2, 0.7) |> round(2),
  sd = runif(n_dfs2, 0.1, 0.4) |> round(2),
  n = runif(n_dfs2, 40, 100) |> round(0)
)

df_grim3 <- tibble::tibble(
  x = c(
    7.22,
    4.74,
    5.23,
    2.57,
    6.77,
    2.68,
    7.01,
    7.38,
    3.14,
    6.89,
    5.00,
    0.24
  ),
  n = c(32, 25, 29, 24, 27, 28, 29, 26, 27, 31, 25, 28),
  success = c(
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    TRUE,
    FALSE
  ),
) |>
  structure(
    class = c(
      "scrutiny_grim_map",
      "scrutiny_rounding_up_or_down",
      "tbl_df",
      "tbl",
      "data.frame"
    )
  )


# Running old and new (= manufactured) functions --------------------------

out_grim_old1 <- grim_map(df_grim1, digits_x = 2) |>
  dplyr::select(x, n, consistency)
out_debit_old1 <- debit_map(df_debit1, digits_x = 2, digits_sd = 2) |>
  dplyr::select(x, sd, n, consistency)

out_grim_new1 <- grim_map_alt(df_grim1, digits_x = 2)
out_debit_new1 <- debit_map_alt(df_debit1, digits_x = 2, digits_sd = 2)

out_grim_old2 <- grim_map(df_grim2, digits_x = 2) |>
  dplyr::select(x, n, consistency)
out_debit_old2 <- debit_map(df_debit2, digits_x = 2, digits_sd = 2) |>
  dplyr::select(x, sd, n, consistency)

out_grim_new2 <- grim_map_alt(df_grim2, digits_x = 2)
out_debit_new2 <- debit_map_alt(df_debit2, digits_x = 2, digits_sd = 2)


out_grim_old_renamed <- out_grim_old1 |>
  dplyr::rename(success = consistency)

out_grim_new_renamed <- grim_map_alt_renamed(df_grim1, digits_x = 2)

out_debit_old_renamed <- out_debit_old1 |>
  dplyr::rename(success = consistency)

out_debit_new_renamed <- debit_map_alt_renamed(
  df_debit1,
  digits_x = 2,
  digits_sd = 2
)


# Testing -----------------------------------------------------------------

test_that("It works for GRIM", {
  out_grim_old1 |> expect_equal(out_grim_new1)
  out_grim_old2 |> expect_equal(out_grim_new2)
})

test_that("It works for DEBIT", {
  out_debit_old1 |> expect_equal(out_debit_new1)
  out_debit_old2 |> expect_equal(out_debit_new2)
})

test_that("Renaming `\"consistency\"` via `.name_key_result` works", {
  out_grim_old_renamed  |> expect_equal(out_grim_new_renamed)
  out_debit_old_renamed |> expect_equal(out_debit_new_renamed)
})

test_that("Wrong `.reported` values throw an error", {
  function_map(
    .fun = grim_scalar,
    .reported = c("x", "success", "n"),
    .name_test = "GRIM"
  ) |> expect_error()
})


# New factory capabilities ------------------------------------------------

test_that("`.args_by_row` allows one value per row and returns a column", {
  df <- tibble::tibble(x = c(1.03, 1.3), sd = c(0.41, 0.41), n = c(40L, 40L))

  # Without `.args_by_row`, a `digits_*` argument is one value for the whole
  # call, as it is for the `*_scalar()` function itself:
  map_const <- function_map(
    .fun = grimmer_scalar,
    .reported = c("x", "sd", "n"),
    .name_test = "GRIMMER"
  )
  map_const(df, digits_x = 2, digits_sd = 2) |>
    colnames() |>
    expect_equal(c("x", "sd", "n", "consistency"))
  map_const(df, digits_x = c(2, 1), digits_sd = 2) |>
    expect_error()

  map_by_row <- function_map(
    .fun = grimmer_scalar,
    .reported = c("x", "sd", "n"),
    .name_test = "GRIMMER",
    .args_by_row = c("digits_x", "digits_sd")
  )
  out <- map_by_row(df, digits_x = c(2, 1), digits_sd = 2)
  out$digits_x |> expect_equal(c(2, 1))
  out$digits_sd |> expect_equal(c(2, 2))
  out |>
    colnames() |>
    expect_equal(c("x", "sd", "n", "digits_x", "digits_sd", "consistency"))
})


test_that("`.col_names` unpacks the test function's values, keeping types", {
  out <- debit_map(pigs3, digits_x = 2, digits_sd = 2)

  out$rounding |> expect_type("character")
  out$consistency |> expect_type("logical")
  out$sd_incl_lower |> expect_type("logical")
  out$sd_lower |> expect_type("double")

  # The same function returns a single value per row if it is not asked to show
  # its reconstructed values, and the factory-made function copes with both:
  debit_map(pigs3, digits_x = 2, digits_sd = 2, show_rec = FALSE) |>
    colnames() |>
    expect_equal(c("x", "sd", "n", "digits_x", "digits_sd", "consistency"))
})


test_that("`.cols_helper` supports helper columns", {
  df <- tibble::tibble(x = 4.67, sd = 0.00, n = 2L, items = 3)

  # `items` may be a column of `data`...
  grimmer_map(df, digits_x = 2, digits_sd = 2)$n |> expect_equal(6L)
  # ...or an argument, but not both if they contradict each other:
  grimmer_map(df, digits_x = 2, digits_sd = 2, items = 5) |> expect_error()
  grimmer_map(pigs5, digits_x = 2, digits_sd = 2, items = 2)$n |>
    expect_equal(as.integer(pigs5$n * 2))
})


test_that("`.args_defaults` overrides the test function's own defaults", {
  # `grimmer_scalar()` has `show_reason = FALSE`, `grimmer_map()` has `TRUE`:
  formals(grimmer_scalar)$show_reason |> expect_false()
  formals(grimmer_map)$show_reason |> expect_true()
  grimmer_map(pigs5, digits_x = 2, digits_sd = 2)$reason |>
    expect_type("character")
})


test_that("arguments of the test function become real arguments", {
  # Not just dots -- see below for why this matters:
  args_grimmer_map <- names(formals(grimmer_map))
  args_debit_map <- names(formals(debit_map))

  c("digits_x", "digits_sd", "rounding", "threshold", "symmetric") |>
    setdiff(args_grimmer_map) |>
    expect_length(0L)

  c("digits_x", "digits_sd", "rounding", "threshold", "symmetric") |>
    setdiff(args_debit_map) |>
    expect_length(0L)

  # Disabled arguments are not among them:
  map_disabled <- function_map(
    .fun = grim_scalar,
    .reported = c("x", "n"),
    .name_test = "GRIM",
    .args_disabled = "percent"
  )
  names(formals(map_disabled)) |> expect_no_match("percent")
  map_disabled(pigs1, digits_x = 2, percent = TRUE) |> expect_error()
})


test_that("the sequence mappers still find their `digits_*` arguments", {
  # `function_map_seq()` and `function_map_total_n()` derive these from the
  # basic mapper's formals. If a `digits_*` argument were only in the mapper's
  # dots, the sequence mapper would silently lose both the argument and the
  # `digits_*` output column that `grim_plot()` reads:
  names(formals(grimmer_map_seq)) |> expect_contains("digits_x")
  names(formals(grimmer_map_seq)) |> expect_contains("digits_sd")
  names(formals(debit_map_seq)) |> expect_contains("digits_x")
  names(formals(debit_map_seq)) |> expect_contains("digits_sd")
  names(formals(grim_map_seq)) |> expect_contains("digits_x")
})


test_that("wrong argument names throw an error at factory time", {
  function_map(
    .fun = grim_scalar,
    .reported = c("x", "n"),
    .name_test = "GRIM",
    .args_by_row = "digits_y"
  ) |>
    expect_error()

  function_map(
    .fun = grim_scalar,
    .reported = c("x", "n"),
    .name_test = "GRIM",
    .cols_helper = "widgets"
  ) |>
    expect_error()

  function_map(
    .fun = grim_scalar,
    .reported = c("x", "n"),
    .name_test = "GRIM",
    .name_class_flags = c(percentage = "scrutiny_percent_true")
  ) |>
    expect_error()

  function_map(
    .fun = grim_scalar,
    .reported = c("x", "n"),
    .name_test = "GRIM",
    .cols_derived = list(probability = "grim_probability")
  ) |>
    expect_error()

  function_map(
    .fun = grim_scalar,
    .reported = c("x", "n"),
    .name_test = "GRIM",
    .cols_derived = list(grim_probability)
  ) |>
    expect_error()
})


test_that("`.cols_derived` computes columns the test function never returns", {
  # `probability` comes from `grim_probability()`, not from `grim_scalar()`:
  out <- grim_map(pigs1, digits_x = 2)
  out$probability |> expect_equal(grim_probability(pigs1$x, pigs1$n, 2))
  # It follows the key result column, ahead of the `.col_names` columns:
  out |>
    colnames() |>
    expect_equal(c("x", "n", "digits_x", "consistency", "probability"))
  grim_map(pigs1, digits_x = 2, show_rec = TRUE) |>
    colnames() |>
    expect_equal(c(
      "x", "n", "digits_x", "consistency", "probability",
      "rec_sum", "sum_lower", "sum_upper", "rec_x_upper", "rec_x_lower"
    ))

  # The derived function only gets the arguments it has formals for.
  # `grim_probability()` has no `rounding`, but `grim_scalar()` does, and
  # `items` and `percent` must reach it:
  grim_map(pigs1, digits_x = 2, rounding = "ceiling")$probability |>
    expect_equal(out$probability)
  grim_map(pigs1, digits_x = 2, items = 2)$probability |>
    expect_equal(grim_probability(pigs1$x, pigs1$n, 2, items = 2))
  grim_map(pigs2, digits_x = 1, percent = TRUE)$probability |>
    expect_equal(grim_probability(pigs2$x, pigs2$n, 1, percent = TRUE))
})


test_that("`.name_class_flags` adds a class when the argument is `TRUE`", {
  grim_map(pigs2, digits_x = 1, percent = TRUE) |>
    expect_s3_class("scrutiny_percent_true")
  grim_map(pigs2, digits_x = 3) |>
    inherits("scrutiny_percent_true") |>
    expect_false()
})


test_that("a mapper called on a 0-row data frame returns a valid tibble", {
  for (out in list(
    grim_map(pigs1[0L, ], digits_x = 2),
    grimmer_map(pigs5[0L, ], digits_x = 2, digits_sd = 2),
    debit_map(pigs3[0L, ], digits_x = 2, digits_sd = 2)
  )) {
    out |> nrow() |> expect_equal(0L)
    # The key result column is present and is a real column, not `NULL`. It
    # used to be the latter, so `ncol()` counted a column that `colnames()` did
    # not name:
    out |> colnames() |> expect_contains("consistency")
    out |> ncol() |> expect_equal(length(colnames(out)))
    out$consistency |> expect_type("logical")
    out |> audit() |> nrow() |> expect_equal(1L)
  }
})
