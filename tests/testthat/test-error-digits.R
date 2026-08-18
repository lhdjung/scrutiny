# The `digits_*` errors name the function the user actually called. Resolving
# that name used to depend on counting frames, which broke whenever the call
# stack had an unexpected shape: `Vectorize()` inserts `do.call()` and
# `mapply()`, purrr inserts several frames of its own, and factory-made
# functions invoke `fun` as a function object, so that frame carries no name.
# `as.character()` on such a call head threw an error, and on a call head like
# `scrutiny::grim_map` it returned a length-3 vector, which `lifecycle` then
# rejected. Either way, the user saw an internal error instead of guidance.

df_grim <- tibble::tibble(x = 4.11, n = 40)
df_grimmer <- tibble::tibble(x = 5.23, sd = 2.55, n = 35)
df_debit <- tibble::tibble(x = 0.35, sd = 0.18, n = 20)

# Extract the error message from `expr` as a single string.
# `error_digits_missing()` also prints a changelog hint via `on.exit()` as it
# unwinds, which is a `message` condition and would otherwise clutter test
# output:
msg_error <- function(expr) {
  conditionMessage(suppressMessages(tryCatch(expr, error = function(e) e)))
}


test_that("a missing `digits_*` argument names the function that was called", {
  # Not aligning pipes here because the lengths are too different
  4.11       |> grim(40)              |> msg_error() |> expect_match("`grim(", fixed = TRUE)
  df_grim    |> grim_map()            |> msg_error() |> expect_match("`grim_map(", fixed = TRUE)
  df_grimmer |> grimmer_map()         |> msg_error() |> expect_match("`grimmer_map(", fixed = TRUE)
  0.35       |> debit(0.18, 20)       |> msg_error() |> expect_match("`debit(", fixed = TRUE)
  df_debit   |> debit_map()           |> msg_error() |> expect_match("`debit_map(", fixed = TRUE)

  # `Vectorize()`, purrr, and the function factories put frames of varying
  # depth between the test function and the check that throws:
  df_grim |>
    grim_map_seq(dispersion = 1:2) |>
    msg_error() |>
    expect_match("`grim_map_seq(", fixed = TRUE)
  df_debit |>
    debit_map_seq(dispersion = 1:2) |>
    msg_error() |>
    expect_match("`debit_map_seq(", fixed = TRUE)
  tibble::tibble(x1 = 4.52, x2 = 4.19, n = 40) |>
    grim_map_total_n() |>
    msg_error() |>
    expect_match("`grim_map_total_n(", fixed = TRUE)
})


test_that("a missing `digits_*` argument gives the intended error", {
  # Not an internal error such as "cannot coerce type 'closure'":
  for (msg in list(
    df_grim |> grim_map_seq(dispersion = 1:2) |> msg_error(),
    df_debit |> debit_map_seq(dispersion = 1:2) |> msg_error()
  )) {
    expect_match(msg, "Need to specify `digits_", fixed = TRUE)
    expect_no_match(msg, "coerce", fixed = TRUE)
  }

  # A namespace-qualified call has a call head that is itself a call, which used
  # to make the name resolve to a length-3 vector:
  df_grim |>
    scrutiny::grim_map() |>
    msg_error() |>
    expect_match("Need to specify `digits_x", fixed = TRUE)
})


test_that("mapper examples build a data frame, basic ones do not", {
  df_grim |> grim_map() |> msg_error() |> expect_match("tibble::tibble(", fixed = TRUE)
  4.11 |> grim(40) |> msg_error() |> expect_no_match("tibble::tibble(", fixed = TRUE)
})


test_that("GRIMMER and DEBIT examples include `sd` and `digits_sd`", {
  msg <- df_grimmer |> grimmer_map() |> msg_error()
  expect_match(msg, "sd = 0.62", fixed = TRUE)
  expect_match(msg, "digits_sd = 2", fixed = TRUE)

  # GRIM has no `sd` argument, so its example must not mention one:
  df_grim |> grim_map() |> msg_error() |> expect_no_match("digits_sd", fixed = TRUE)
})


test_that("string `x` is rejected with a deprecation message", {
  tibble::tibble(x = "4.11", n = 40) |>
    grim_map(digits_x = 2) |>
    msg_error() |>
    expect_match("scrutiny::grim_map()", fixed = TRUE)
  # The name comes from the outermost test function, not the `grim_map()` that
  # a sequence mapper calls internally:
  tibble::tibble(x = "4.11", n = 40) |>
    grim_map_seq(digits_x = 2, dispersion = 1:2) |>
    msg_error() |>
    expect_match("scrutiny::grim_map_seq()", fixed = TRUE)
})


test_that("`digits_*` below the decimal places of `x` is an error", {
  4.111 |>
    grim(40, digits_x = 2) |>
    msg_error() |>
    expect_match("More decimal places than specified digits", fixed = TRUE)
  tibble::tibble(x = 4.111, n = 40) |>
    grim_map(digits_x = 2) |>
    msg_error() |>
    expect_match("More decimal places than specified digits", fixed = TRUE)
})
