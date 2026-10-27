df1 <- tibble::tibble(
  y1 = 16:25,
  y2 = 26:35,
  n = seq(from = 12, to = 21, by = 1)
)


# First, create a mock consistency test, called SCHLIM. It's analogous to GRIM
# as implemented in scrutiny, which is also true for the function names:
schlim_scalar <- function(y, n) {
  # Note: `grim_scalar()` is not exported
  y / 3 > n
}

# Not needed below, but included for completeness:
schlim <- Vectorize(schlim_scalar)

# This will be the input function:
schlim_map <- function(data) {
  consistency <- purrr::map2_lgl(
    as.numeric(data$y),
    as.numeric(data$n),
    schlim_scalar
  )
  data |> dplyr::mutate(consistency)
}

# Use the function factory:
schlim_map_total_n <- function_map_total_n(
  .fn = schlim_map,
  .reported = "y",
  .name_test = "SCHLIM",
  .name_class = "scrutiny_schlim_map_total_n"
)


# Apply the manufactured function to `df1`:
df1_tested <- schlim_map_total_n(df1)


# Conduct tests:
test_that("The manufactured function's output
          has the correct dimensions", {
  # Five of the ten totals are even, and the 50/50 split of an even total is
  # tested in one direction only:
  df1_tested |> dim() |> expect_equal(c(230, 7))
})


df1_tested_forth <- df1_tested |> dplyr::filter(dir == "forth")
df1_tested_back <- df1_tested |> dplyr::filter(dir == "back")

df1_tested_case1 <- df1_tested |> dplyr::filter(case == 1)
df1_tested_case2 <- df1_tested |> dplyr::filter(case == 2)
df1_tested_case3 <- df1_tested |> dplyr::filter(case == 3)


test_that("It has the correct dimensions when split by `dir`", {
  df1_tested_forth |> dim() |> expect_equal(c(120, 7))
  df1_tested_back  |> dim() |> expect_equal(c(110, 7))
})

test_that("It has the correct dimensions when split by `case`", {
  df1_tested_case1 |> dim() |> expect_equal(c(22, 7))
  df1_tested_case2 |> dim() |> expect_equal(c(24, 7))
  df1_tested_case3 |> dim() |> expect_equal(c(22, 7))
})


vals_exp_y <- c(16, 26, 16, 26, 16, 26)
vals_exp_n <- c(6, 6, 5, 7, 4, 8)

f <- FALSE
t <- TRUE
vals_exp_consistency <- c(f, t, t, t, t, t)

test_that("Judging by a small sample, it has correct values", {
  df1_tested$y[1:6] |> expect_equal(vals_exp_y)
  df1_tested$n[1:6] |> expect_equal(vals_exp_n)
  df1_tested$consistency[1:6] |> expect_equal(vals_exp_consistency)
})


test_that("the column order of `data` makes no difference", {
  # The key columns in reverse order. The "back" direction swaps the `y1` and
  # `y2` values by renaming and reordering columns, and the swap used to be
  # silently skipped -- returning "forth" results labeled "back" -- whenever
  # the key columns were not in the exact `y1, y2` order:
  df1_reversed <- df1[c("y2", "y1", "n")]
  df1_reversed |> schlim_map_total_n() |> expect_identical(df1_tested)
})

test_that("the \"back\" direction really swaps the group pairings", {
  # `y1` values reappear as `y` in the "back" half's even rows (group 2), and
  # `y2` values in its odd rows (group 1) -- the reverse of "forth":
  df1_tested_back$y[c(TRUE, FALSE)] |> unique() |> expect_identical(df1$y2)
  df1_tested_back$y[c(FALSE, TRUE)] |> unique() |> expect_identical(df1$y1)
})


test_that("group suffixes are swapped even if the statistic's name has digits", {
  # The swap used to run `stringr::str_replace()` on the whole column name, so
  # it hit the wrong character as soon as the reported statistic was called
  # something like "t1", whose columns are `t11` and `t12`.
  t1_scalar <- function(t1, n, digits_t1) {
    abs(t1 * n - round(t1 * n)) < 10^-digits_t1
  }
  t1_map <- function_map(
    .fn = t1_scalar,
    .reported = c("t1", "n"),
    .args_by_row = "digits_t1",
    .name_test = "T1"
  )
  t1_map_total_n <- function_map_total_n(
    .fn = t1_map,
    .reported = "t1",
    .name_test = "T1"
  )
  out <- t1_map_total_n(
    tibble::tibble(t11 = 0.5, t12 = 0.25, n = 8),
    digits_t1 = 2
  )
  # In the "back" direction the two reported values change places, so the first
  # row of that half must carry `t12`'s value:
  out$t1[out$dir == "forth"][1L] |> expect_equal(0.5)
  out$t1[out$dir == "back"][1L]  |> expect_equal(0.25)
})


test_that("the output carries a test-specific class", {
  # `?audit-special` documents `"scrutiny_grim_map_total_n"` and friends, and
  # the seq tier has always set its counterpart. The total-n tier only set the
  # generic `"scrutiny_map_total_n"`, so it was the one tier whose output could
  # not be dispatched on by test:
  tibble::tibble(x1 = 4.52, x2 = 5.23, n = 40L) |>
    grim_map_total_n(digits_x = 2, items_averaged = 1) |>
    expect_s3_class("scrutiny_grim_map_total_n")
  tibble::tibble(x1 = 4.52, x2 = 5.23, sd1 = 1.36, sd2 = 1.19, n = 40L) |>
    grimmer_map_total_n(digits_x = 2, digits_sd = 2, items_averaged = 1) |>
    expect_s3_class("scrutiny_grimmer_map_total_n")
  tibble::tibble(x1 = 0.30, x2 = 0.28, sd1 = 0.17, sd2 = 0.10, n = 70L) |>
    debit_map_total_n(digits_x = 2, digits_sd = 2) |>
    expect_s3_class("scrutiny_debit_map_total_n")
  # The generic class that `audit_total_n()` dispatches on is still there:
  tibble::tibble(x1 = 4.52, x2 = 5.23, n = 40L) |>
    grim_map_total_n(digits_x = 2, items_averaged = 1) |>
    expect_s3_class("scrutiny_map_total_n")
})


test_that("`.name_key_result` renames the key result column", {
  # It is a documented parameter of all three factories, but the seq and
  # total-n ones used to hard-code `"consistency"` and fail with base-R errors
  # ("first argument must be a vector", "invalid argument type"):
  vermin_map <- function_map(
    .fn = function(y, n) (y / 3) > n,
    .reported = c("y", "n"),
    .name_test = "VERMIN",
    .name_key_result = "verdict"
  )
  vermin_map_total_n <- function_map_total_n(
    .fn = vermin_map,
    .reported = "y",
    .name_test = "VERMIN",
    .name_key_result = "verdict"
  )
  out <- vermin_map_total_n(tibble::tibble(y1 = 16, y2 = 18, n = 20))
  out |> colnames() |> expect_contains(c("verdict", "both_consistent"))
  out |> rlang::has_name("consistency") |> expect_false()
  out$verdict |> expect_type("logical")
  out |> audit_total_n() |> nrow() |> expect_equal(1L)

  vermin_map_seq <- function_map_seq(
    .fn = vermin_map,
    .reported = c("y", "n"),
    .name_test = "VERMIN",
    .name_key_result = "verdict"
  )
  out_seq <- vermin_map_seq(tibble::tibble(y = 16:25, n = 3:12))
  out_seq |> colnames() |> expect_contains("verdict")
  out_seq |> rlang::has_name("consistency") |> expect_false()
  out_seq |> audit_seq() |> colnames() |> expect_contains("verdict")

  # A mismatch between the two factories' `.name_key_result` values is caught
  # with a message that names the column, rather than an obscure `NULL`:
  mismatched <- function_map_seq(
    .fn = vermin_map,
    .reported = c("y", "n"),
    .name_test = "SCHLIM"
  )
  tibble::tibble(y = 16:25, n = 3:12) |>
    mismatched() |>
    expect_error("did not return a \"consistency\" column")
})


test_that("`digits_*` is a real formal of the total-n mappers", {
  # It used to reach them through the dots only. That worked, and the
  # missing-argument error was still the bespoke one, but the argument was
  # invisible to `formals()`, to tab-completion, and to the rendered help page
  # -- despite having no default and being required in every call.
  grim_map_total_n    |> formals() |> names() |> head(3) |>
    expect_equal(c("data", "digits_x", "x1"))
  grimmer_map_total_n |> formals() |> names() |> head(5) |>
    expect_equal(c("data", "digits_x", "digits_sd", "x1", "x2"))
  debit_map_total_n   |> formals() |> names() |> head(5) |>
    expect_equal(c("data", "digits_x", "digits_sd", "x1", "x2"))

  # Passing it positionally now works, as it does for the other two tiers:
  df <- tibble::tibble(x1 = 4.52, x2 = 5.23, n = 40L)
  df |>
    grim_map_total_n(2, dispersion = 0:1, items_averaged = 1) |>
    expect_identical(grim_map_total_n(df, digits_x = 2, items_averaged = 1, dispersion = 0:1))

  # ...and omitting it still gives the bespoke message, not a generic one:
  df |> grim_map_total_n() |> expect_error("Need to specify `digits_x`")
  tibble::tibble(x1 = 4.52, x2 = 5.23, sd1 = 1.36, sd2 = 1.19, n = 40L) |>
    grimmer_map_total_n() |>
    expect_error("Need to specify `digits_x`")
})


test_that("a missing total `n` is reported as such, not as a decimal number", {
  list(x1 = 4.52, x2 = 5.23, n = NA) |> 
    tibble::new_tibble(nrow = 1L) |> 
    grim_map_total_n(digits_x = 2, items_averaged = 1) |>
    expect_error("values that are not")
})


test_that("A total below 2 and 0-row `data` get their own errors", {
  tibble::tibble(x1 = 3.43, x2 = 3.51, n = 1) |>
    grim_map_total_n(digits_x = 2, items_averaged = 1) |>
    expect_error("at least 2")
  tibble::tibble(x1 = numeric(), x2 = numeric(), n = numeric()) |>
    grim_map_total_n(digits_x = 2, items_averaged = 1) |>
    expect_error("at least one row")
})


test_that("total-n mappers take a single helper value, carry other columns, and report dropped cases", {
  df <- tibble::tibble(x1 = c(5.19, 3.2), x2 = c(4.1, 2.2), n = c(40, 100), id = c("a", "b"))
  df |> grim_map_total_n(digits_x = 2, items_averaged = c(1, 2)) |> expect_error("single value")
  df |> grim_map_total_n(digits_x = 2, items_averaged = 1, dispersion = 0) |> dplyr::pull(id) |> expect_equal(c("a", "a", "b", "b"))
  df |> dplyr::mutate(dir = 1) |> grim_map_total_n(digits_x = 2, items_averaged = 1) |> expect_error("would be overwritten")
  df |> dplyr::mutate(x = 5.21) |> grim_map_total_n(digits_x = 2, items_averaged = 1) |> expect_error("`x` would be overwritten")
  df |> dplyr::mutate(sd1 = 1, sd2 = 1, sd = 9) |> grimmer_map_total_n(digits_x = 2, digits_sd = 0, items_averaged = 1) |> expect_error("`sd` would be overwritten")
  df |> grim_map_total_n(digits_x = 2, items_averaged = 1, n_max = 30) |> expect_warning("no pair of group sizes")
})


test_that("GRIMMER and DEBIT total-n mappers never test a group of 1", {
  # Such rows had no test result, and `audit_total_n()` counted them:
  out_grimmer <- tibble::tibble(x1 = 5.2, x2 = 4.25, sd1 = 1.3, sd2 = 1.4, n = 8) |> grimmer_map_total_n(digits_x = 2, digits_sd = 2, items_averaged = 1)
  out_debit   <- tibble::tibble(x1 = 0.5, x2 = 0.25, sd1 = 0.58, sd2 = 0.5, n = 8) |> debit_map_total_n(digits_x = 2, digits_sd = 2)
  out_grimmer$n |> min() |> expect_equal(2L)
  out_debit$n   |> min() |> expect_equal(2L)
})


test_that("`constant` values stay with their groups in both directions", {
  # In the "back" direction, they used to stay with the group positions:
  out <- tibble::tibble(x1 = 3.43, x2 = 5.28, n = 90) |> grim_map_total_n(digits_x = 2, items_averaged = 1, dispersion = 0:1, constant = c("ctrl", "treat"))
  out$constant[out$x == 3.43] |> unique() |> expect_equal("ctrl")
  out$constant[out$x == 5.28] |> unique() |> expect_equal("treat")
  out_list <- tibble::tibble(x1 = 3.43, x2 = 5.28, n = 90) |> grim_map_total_n(digits_x = 2, items_averaged = 1, dispersion = 0:1, constant = list(group = c("ctrl", "treat")))
  out_list$group[out_list$x == 5.28] |> unique() |> expect_equal("treat")
})


test_that("sequence and total-n mappers take the settings of their basic mapper as arguments", {
  # They used to go through the dots, invisible to `formals()` and the help
  # page. Only `items_averaged`, a helper, is not a setting:
  pairs <- list(
    list(grim_map,             grim_map_seq),
    list(grimmer_map,          grimmer_map_seq),
    list(debit_map,            debit_map_seq),
    list(is_percentage_of_map, is_percentage_of_map_seq),
    list(grim_map,             grim_map_total_n),
    list(grimmer_map,          grimmer_map_total_n),
    list(debit_map,            debit_map_total_n)
  )
  for (pair in pairs) {
    settings <- pair[[1L]] |> formals_settings(exclude = "items_averaged")
    settings |> length() |> expect_gt(2L)
    formals(pair[[2L]])[names(settings)] |> expect_equal(settings)
  }
})

test_that("a promoted setting reaches the test only if it was given", {
  out <- pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1, rounding = "up")
  out |> scrutiny_args() |> _$rounding |> expect_equal("up")

  total <- tibble::tibble(x1 = 0.30, x2 = 0.30, sd1 = 0.46, sd2 = 0.46, n = 20)
  total |> debit_map_total_n(digits_x = 2, digits_sd = 2, formula = "mean_n") |> scrutiny_args() |> _$formula |> expect_equal("mean_n")
  total |> debit_map_total_n(digits_x = 2, digits_sd = 2)                     |> scrutiny_args() |> _$formula |> expect_equal("exact")
})


test_that("an error names the row of `data`, not of the scenarios", {
  tibble::tibble(x1 = c(3.8, 4.5, 3.1), x2 = c(3.7, 4.56, 2.1), n = c(30, 40, 20)) |>
    grim_map_total_n(digits_x = 1, items_averaged = 1) |>
    expect_error("In row 2 of `data`", fixed = TRUE)
  # The first scenario is tested on its own, before all others:
  tibble::tibble(x1 = c(4.5, 3.8), x2 = c(4.56, 3.7), n = c(40, 30)) |>
    grim_map_total_n(digits_x = 1, items_averaged = 1) |>
    expect_error("In row 1 of `data`", fixed = TRUE)
})
