# Manufactured functions --------------------------------------------------

# Stripped-down versions of the real mappers: same `.fn` and `.reported`, but
# none of the arguments that give the real ones their extra columns. They exist
# to check that the factory's core -- key columns, renaming, the key result
# column -- is what the real mappers get from it, and that the extras really are
# extras. The `"scrutiny"` attribute is ignored in the comparisons: it records
# the arguments that applied to the whole call, and these differ by design.

grim_map_alt <- function_map(
  .fn = grim_scalar,
  .reported = c("x", "n"),
  .name_test = "GRIM"
)

debit_map_alt <- function_map(
  .reported = c("x", "sd", "n"),
  .fn = debit_scalar,
  .name_test = "DEBIT"
)

grim_map_alt_renamed <- function_map(
  .fn = grim_scalar,
  .reported = c("x", "n"),
  .name_test = "GRIM",
  .name_key_result = "success"
)

debit_map_alt_renamed <- function_map(
  .reported = c("x", "sd", "n"),
  .fn = debit_scalar,
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
      "tbl_df",
      "tbl",
      "data.frame"
    )
  )


# Running old and new (= manufactured) functions --------------------------

out_grim_old1 <- df_grim1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  dplyr::select(x, n, consistency)
out_debit_old1 <- df_debit1 |>
  debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
  dplyr::select(x, sd, n, consistency)

out_grim_new1  <- df_grim1  |> grim_map_alt(digits_x = 2, items_averaged = 1, data_type = "integer") # fmt: skip
out_debit_new1 <- df_debit1 |> debit_map_alt(digits_x = 2, digits_sd = 2, data_type = "integer") # fmt: skip

out_grim_old2 <- df_grim2 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  dplyr::select(x, n, consistency)
out_debit_old2 <- df_debit2 |>
  debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
  dplyr::select(x, sd, n, consistency)

out_grim_new2  <- df_grim2  |> grim_map_alt(digits_x = 2, items_averaged = 1, data_type = "integer") # fmt: skip
out_debit_new2 <- df_debit2 |> debit_map_alt(digits_x = 2, digits_sd = 2, data_type = "integer") # fmt: skip


out_grim_old_renamed <- out_grim_old1 |>
  dplyr::rename(success = consistency)

out_grim_new_renamed <- df_grim1 |>
  grim_map_alt_renamed(digits_x = 2, items_averaged = 1, data_type = "integer")

out_debit_old_renamed <- out_debit_old1 |>
  dplyr::rename(success = consistency)

out_debit_new_renamed <- df_debit1 |>
  debit_map_alt_renamed(digits_x = 2, digits_sd = 2, data_type = "integer")


# Testing -----------------------------------------------------------------

test_that("It works for GRIM", {
  out_grim_old1 |> expect_equal(out_grim_new1, ignore_attr = "scrutiny")
  out_grim_old2 |> expect_equal(out_grim_new2, ignore_attr = "scrutiny")
})

test_that("It works for DEBIT", {
  out_debit_old1 |> expect_equal(out_debit_new1, ignore_attr = "scrutiny")
  out_debit_old2 |> expect_equal(out_debit_new2, ignore_attr = "scrutiny")
})

test_that("Renaming `\"consistency\"` via `.name_key_result` works", {
  out_grim_old_renamed  |>
    expect_equal(out_grim_new_renamed, ignore_attr = "scrutiny")
  out_debit_old_renamed |>
    expect_equal(out_debit_new_renamed, ignore_attr = "scrutiny")
})

test_that("a `consistency` column is not silently lost under another name", {
  v_map <- function_map(
    .fn = grim_scalar,
    .reported = c("x", "n"),
    .name_test = "VGRIM",
    .name_key_result = "verdict",
    .args_by_row = "digits_x"
  )
  pigs1[1:3, ] |>
    dplyr::mutate(consistency = "keep me") |>
    v_map(digits_x = 2) |>
    expect_error("already includes a \"consistency\" column")
})

test_that("Wrong `.reported` values throw an error", {
  grim_scalar |>
    function_map(
      .reported = c("x", "success", "n"),
      .name_test = "GRIM"
    ) |>
    expect_error()
})


# New factory capabilities ------------------------------------------------

test_that("`.args_by_row` allows one value per row and returns a column", {
  df <- tibble::tibble(x = c(1.03, 1.3), sd = c(0.41, 0.41), n = c(40L, 40L))

  # Without `.args_by_row`, a `digits_*` argument is one value for the whole
  # call, as it is for the `*_scalar()` function itself:
  map_const <- function_map(
    .fn = grimmer_scalar,
    .reported = c("x", "sd", "n"),
    .name_test = "GRIMMER"
  )
  df |> map_const(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    colnames() |>
    expect_equal(c("x", "sd", "n", "consistency"))
  df |> map_const(digits_x = c(2, 1), digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_error()

  map_by_row <- function_map(
    .fn = grimmer_scalar,
    .reported = c("x", "sd", "n"),
    .name_test = "GRIMMER",
    .args_by_row = c("digits_x", "digits_sd")
  )
  out <- df |> map_by_row(digits_x = c(2, 1), digits_sd = 2, items_averaged = 1, data_type = "integer")
  out$digits_x |> expect_equal(c(2, 1))
  out$digits_sd |> expect_equal(c(2, 2))
  out |>
    colnames() |>
    expect_equal(c("x", "sd", "n", "digits_x", "digits_sd", "consistency"))
})


test_that("`.col_names` unpacks the test function's values, keeping types", {
  out <- pigs3 |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer", show_rec = TRUE)

  out$consistency |> expect_type("logical")
  out$sd_incl_lower |> expect_type("logical")
  out$sd_lower |> expect_type("double")
})


test_that("`.cols_helper` supports helper columns", {
  df <- tibble::tibble(x = 4.67, sd = 0.00, n = 2L, items_averaged = 3)

  # `items_averaged` may be a column of `data`, returned as itself right after
  # `n`...
  out <- df |> grimmer_map(digits_x = 2, digits_sd = 2, data_type = "integer")
  out |> colnames() |> expect_equal(c("x", "sd", "n", "items_averaged", "data_type", "digits_x", "digits_sd", "consistency", "reason"))
  out$n              |> expect_equal(2L)
  out$items_averaged |> expect_equal(3)
  # ...or an argument, which must agree with the column if both are given:
  df |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 5, data_type = "integer") |> expect_error("already in")
  df |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 3, data_type = "integer") |> expect_equal(out)
  out <- pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 2, data_type = "integer")
  out$n              |> expect_equal(pigs5$n)
  out$items_averaged |> expect_equal(rep(2, nrow(pigs5)))
  # Neither is an error, since there is no default:
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, data_type = "integer") |> expect_error("items_averaged")
})


test_that("`.args_fixed` passes a value on without exposing the argument", {
  # `grimmer_scalar()` has `show_reason = FALSE`, `grimmer_map()` fixes `TRUE`:
  grimmer_scalar |> formals() |> call_on(\(x) x$show_reason) |> expect_false()
  grimmer_map    |> formals() |> names() |> expect_no_match("show_reason")
  pigs5 |>
    grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("reason") |>
    expect_type("character")
  pigs5 |>
    grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", show_reason = FALSE) |>
    expect_error("disabled")
  # A fixed argument is a setting of the factory, not of the call:
  pigs5 |>
    grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    scrutiny_args() |>
    names() |>
    expect_no_match("show_reason")
})


test_that("arguments of the test function become real arguments", {
  # Not just dots -- see below for why this matters:
  args_grimmer_map <- grimmer_map |> formals() |> names()
  args_debit_map   <- debit_map   |> formals() |> names()

  c("digits_x", "digits_sd", "rounding", "threshold", "symmetric") |>
    setdiff(args_grimmer_map) |>
    expect_length(0L)

  c("digits_x", "digits_sd", "rounding", "threshold", "symmetric") |>
    setdiff(args_debit_map) |>
    expect_length(0L)

  # Disabled arguments are not among them:
  map_disabled <- function_map(
    .fn = grim_scalar,
    .reported = c("x", "n"),
    .name_test = "GRIM",
    .args_disabled = "x_meaning"
  )
  map_disabled |> formals() |> names() |> expect_no_match("x_meaning")
  pigs1 |> map_disabled(digits_x = 2, x_meaning = "percentage") |> expect_error()
})


test_that("the `digits_*` arguments come right after `data`", {
  # They have no defaults and must be given in every call, so they sit next to
  # the other argument that must, ahead of the key arguments -- and in the same
  # position in every mapper, basic and sequence alike:
  grim_map        |> formals() |> names() |> head(2) |>
    expect_equal(c("data", "digits_x"))
  grimmer_map     |> formals() |> names() |> head(3) |>
    expect_equal(c("data", "digits_x", "digits_sd"))
  debit_map       |> formals() |> names() |> head(3) |>
    expect_equal(c("data", "digits_x", "digits_sd"))
  grim_map_seq    |> formals() |> names() |> head(2) |>
    expect_equal(c("data", "digits_x"))
  grimmer_map_seq |> formals() |> names() |> head(3) |>
    expect_equal(c("data", "digits_x", "digits_sd"))
  debit_map_seq   |> formals() |> names() |> head(3) |>
    expect_equal(c("data", "digits_x", "digits_sd"))
})


test_that("the sequence mappers still find their `digits_*` arguments", {
  # `function_map_seq()` and `function_map_total_n()` derive these from the
  # basic mapper's formals. If a `digits_*` argument were only in the mapper's
  # dots, the sequence mapper would silently lose both the argument and the
  # `digits_*` output column that `grim_plot_grid()` reads:
  grimmer_map_seq |> formals() |> names() |> expect_contains("digits_x")
  grimmer_map_seq |> formals() |> names() |> expect_contains("digits_sd")
  debit_map_seq   |> formals() |> names() |> expect_contains("digits_x")
  debit_map_seq   |> formals() |> names() |> expect_contains("digits_sd")
  grim_map_seq    |> formals() |> names() |> expect_contains("digits_x")
})


test_that("`.reported` may name any number of key columns", {
  # Nothing in the factory hardcodes the number of key columns: GRIM has two,
  # GRIMMER and DEBIT have three, and a test with more works the same way. The
  # number has to be known when the factory runs, not when the mapper is
  # called (#42).
  quadrant_scalar <- function(a, b, c, d, tolerance = 0) {
    abs((a + b) - (c + d)) <= tolerance
  }

  quadrant_map <- function_map(
    .fn = quadrant_scalar,
    .reported = c("a", "b", "c", "d"),
    .name_test = "QUADRANT"
  )

  quadrant_map |> formals() |> names() |>
    expect_equal(c("data", "a", "b", "c", "d", "...", "tolerance"))

  df <- tibble::tibble(a = 1:3, b = 4:6, c = c(5L, 7L, 9L), d = c(0L, 0L, 1L))
  out <- df |> quadrant_map()
  out |> expect_s3_class("scrutiny_quadrant_map")
  out$consistency |> expect_equal(c(TRUE, TRUE, FALSE))
  out |> colnames() |> expect_equal(c("a", "b", "c", "d", "consistency"))

  # Key-column renaming covers all four of them:
  df_renamed <- df |> dplyr::rename(alpha = a, delta = d)
  df_renamed |> quadrant_map(a = alpha, d = delta) |> expect_equal(out)
  df_renamed |> quadrant_map(a = alpha) |> expect_error()

  # And so does the arity-agnostic column check:
  df |> dplyr::select(-d) |> quadrant_map() |> expect_error()
})


test_that("`.reported_variadic` decides the number of key columns at call time", {
  # The other case of #42: a test over "any number of columns", where how many
  # there are is a property of the caller's data rather than of the factory
  # call. Its `.fn` takes them as one vector argument.
  sum_check_scalar <- function(parts, total, tolerance = 0) {
    abs(sum(parts) - total) <= tolerance
  }

  sum_check_map <- function_map(
    .fn = sum_check_scalar,
    .reported = "total",
    .reported_variadic = "parts",
    .name_test = "SUMCHECK"
  )

  # The variadic argument comes ahead of the fixed key arguments, and it has no
  # default, unlike them:
  sum_check_map |> formals() |> names() |>
    expect_equal(c("data", "parts", "total", "...", "tolerance"))
  sum_check_map |>
    formals() |>
    call_on(\(x) x$parts) |>
    rlang::is_missing() |>
    expect_true()

  df <- tibble::tibble(
    item_1 = c(10, 20, 30),
    item_2 = c(5, 5, 5),
    item_3 = c(1, 2, 3),
    total = c(16, 27, 40),
    note = c("a", "b", "c")
  )

  out <- df |> sum_check_map(parts = c(item_1, item_2, item_3))
  out |> expect_s3_class("scrutiny_sumcheck_map")
  out$consistency |> expect_equal(c(TRUE, TRUE, FALSE))

  # The selected columns are returned as themselves, so the output is as
  # rectangular as any other mapper's:
  out |>
    colnames() |>
    expect_equal(c("item_1", "item_2", "item_3", "total", "consistency", "note"))

  # Any tidyselect expression will do, and the number of columns it picks is
  # the number of values that each test gets:
  df |> sum_check_map(parts = starts_with("item")) |>
    expect_equal(out)
  df |>
    sum_check_map(parts = c(item_1, item_2)) |>
    purrr::pluck("consistency") |>
    expect_equal(c(FALSE, FALSE, FALSE))
  df |>
    sum_check_map(parts = c(item_1, item_2), tolerance = 100) |>
    purrr::pluck("consistency") |>
    expect_equal(c(TRUE, TRUE, TRUE))

  # A column that the selection leaves out is an ordinary other column:
  df |> sum_check_map(parts = c(item_1, item_2)) |>
    colnames() |>
    expect_contains("item_3")

  # The fixed key column still supports renaming, and 0 rows still work:
  df_renamed <- df |> dplyr::rename(sum_col = total)
  df_renamed |> sum_check_map(parts = starts_with("item"), total = sum_col) |>
    expect_equal(out)
  df[0L, ] |> sum_check_map(parts = starts_with("item")) |>
    nrow() |>
    expect_equal(0L)
})


test_that("a variadic key argument must be specified, and must select real columns", {
  sum_check_scalar <- function(parts, total) sum(parts) == total
  sum_check_map <- function_map(
    .fn = sum_check_scalar,
    .reported = "total",
    .reported_variadic = "parts",
    .name_test = "SUMCHECK"
  )
  df <- tibble::tibble(a = 1, b = 2, total = 3)

  # No default: guessing the columns would quietly test the wrong ones.
  df |> sum_check_map() |> expect_error("must be specified")
  df |> sum_check_map(parts = c(a, nonexistent)) |> expect_error("doesn't exist")

  # An empty selection leaves the test function with no values. Without this
  # check, `purrr::pmap()` reports a recycling failure over a variable the
  # caller has never heard of:
  df |> sum_check_map(parts = c()) |> expect_error("selected no columns")

  # A column that has a role already cannot also be tested as one of many
  # values: it would appear twice in the output, giving a tibble with
  # duplicate column names.
  df |> sum_check_map(parts = c(a, total)) |> expect_error("role in the test")
  df |> sum_check_map(parts = everything()) |> expect_error("role in the test")
  df |> sum_check_map(parts = !total) |> expect_no_error()

  # Helper columns count as spoken for, too:
  helper_scalar <- function(parts, total, items = 1) sum(parts) == total * items
  helper_map <- function_map(
    .fn = helper_scalar,
    .reported = "total",
    .reported_variadic = "parts",
    .name_test = "SUMCHECK",
    .cols_helper = "items"
  )
  df_items <- tibble::tibble(a = 1, b = 2, total = 3, items = 1)
  df_items |> helper_map(parts = everything()) |> expect_error("role in the test")
  df_items |>
    helper_map(parts = c(a, b)) |>
    purrr::pluck("consistency") |>
    expect_true()
})


test_that("`.reported` may be empty if `.reported_variadic` is not", {
  all_equal_scalar <- function(values) length(unique(values)) == 1L
  all_equal_map <- function_map(
    .fn = all_equal_scalar,
    .reported_variadic = "values",
    .name_test = "ALLEQUAL"
  )
  all_equal_map |> formals() |> names() |> expect_equal(c("data", "values", "..."))

  df <- tibble::tibble(a = c(1, 2), b = c(1, 3), c = c(1, 3), id = c("x", "y"))
  out <- df |> all_equal_map(values = c(a, b, c))
  out$consistency |> expect_equal(c(TRUE, FALSE))
  out |> colnames() |> expect_equal(c("a", "b", "c", "consistency", "id"))

  # But one of the two must be there:
  all_equal_scalar |>
    function_map(.name_test = "ALLEQUAL") |>
    expect_error("at least one column")
})


test_that("`.reported_variadic` composes with the factory's other arguments", {
  sum_check_scalar <- function(parts, total, digits_total, show_rec = FALSE) {
    gap <- sum(parts) - total
    consistency <- abs(gap) < 10^-digits_total
    if (show_rec) list(consistency, sum(parts), gap) else consistency
  }
  count_parts <- function(parts) lengths(parts)

  sum_check_map <- function_map(
    .fn = sum_check_scalar,
    .reported = "total",
    .reported_variadic = "parts",
    .name_test = "SUMCHECK",
    .args_by_row = "digits_total",
    .args_fixed = list(show_rec = TRUE),
    .col_names = c("consistency", "rec_sum", "gap"),
    .cols_derived = list(n_parts = count_parts)
  )

  # The by-row arguments still come first, ahead of the key arguments:
  sum_check_map |>
    formals() |>
    names() |>
    head(4) |>
    expect_equal(c("data", "digits_total", "parts", "total"))

  df <- tibble::tibble(
    i1 = c(1.5, 2.5),
    i2 = c(2.5, 2.5),
    total = c(4.0, 5.5)
  )
  out <- df |> sum_check_map(digits_total = 1, parts = c(i1, i2))
  out |>
    colnames() |>
    expect_equal(c(
      "i1", "i2", "total", "digits_total",
      "consistency", "n_parts", "rec_sum", "gap"
    ))
  out$consistency |> expect_equal(c(TRUE, FALSE))
  out$rec_sum |> expect_equal(c(4, 5))
  # `.cols_derived` gets each row's values as one vector, like the test itself:
  out$n_parts |> expect_equal(c(2L, 2L))
  df |>
    sum_check_map(digits_total = c(1, 2), parts = c(i1, i2)) |>
    purrr::pluck("digits_total") |>
    expect_equal(c(1, 2))
})


test_that("wrong argument names throw an error at factory time", {
  grim_scalar |>
    function_map(
      .reported = c("x", "n"),
      .name_test = "GRIM",
      .args_by_row = "digits_y"
    ) |>
    expect_error()

  grim_scalar |>
    function_map(
      .reported = c("x", "n"),
      .name_test = "GRIM",
      .cols_helper = "widgets"
    ) |>
    expect_error()

  grim_scalar |>
    function_map(
      .reported = c("x", "n"),
      .name_test = "GRIM",
      .cols_derived = list(exp_incon = "grim_exp_incon")
    ) |>
    expect_error()

  grim_scalar |>
    function_map(
      .reported = c("x", "n"),
      .name_test = "GRIM",
      .cols_derived = list(grim_exp_incon)
    ) |>
    expect_error()

  grim_scalar |>
    function_map(
      .reported = c("x", "n"),
      .name_test = "GRIM",
      .reported_variadic = "values"
    ) |>
    expect_error()

  # A key argument takes either one column's values or those of any number of
  # columns, not both:
  grim_scalar |>
    function_map(
      .reported = c("x", "n"),
      .name_test = "GRIM",
      .reported_variadic = "x"
    ) |>
    expect_error("must not also be")
})


test_that("`.cols_derived` computes columns the test function never returns", {
  # `exp_incon` comes from `grim_exp_incon()`, not from `grim_scalar()`:
  out <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  out$exp_incon |> expect_equal(grim_exp_incon(pigs1$x, pigs1$n, 2, items_averaged = 1, data_type = "integer"))
  # It follows the key result column, ahead of the `.col_names` columns:
  out |>
    colnames() |>
    expect_equal(c("x", "n", "items_averaged", "data_type", "digits_x", "consistency", "exp_incon"))
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", show_rec = TRUE) |>
    colnames() |>
    expect_equal(c(
      "x", "n", "items_averaged", "data_type", "digits_x", "consistency", "exp_incon",
      "rec_sum", "sum_lower", "sum_upper", "rec_x_upper", "rec_x_lower"
    ))

  # The derived function only gets the arguments it has formals for, and
  # `rounding`, `items_averaged`, and `x_meaning` must reach it:
  pigs1 |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "ceiling") |>
    purrr::pluck("exp_incon") |>
    expect_equal(grim_exp_incon(pigs1$x, pigs1$n, 2, rounding = "ceiling", items_averaged = 1, data_type = "integer"))
  pigs1 |>
    grim_map(digits_x = 2, items_averaged = 2, data_type = "integer") |>
    purrr::pluck("exp_incon") |>
    expect_equal(grim_exp_incon(pigs1$x, pigs1$n, 2, items_averaged = 2, data_type = "integer"))
  pigs2 |>
    grim_map(digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |>
    purrr::pluck("exp_incon") |>
    expect_equal(grim_exp_incon(pigs2$x, pigs2$n, 1, x_meaning = "percentage", items_averaged = 1, data_type = "integer"))
})


test_that("the arguments of the whole call are recorded, with defaults", {
  args <- pigs2 |>
    grim_map(digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = "percentage", rounding = "ceiling") |>
    scrutiny_args()
  args$x_meaning |> expect_equal("percentage")
  args$rounding |> expect_equal("ceiling")
  args$threshold |> expect_equal(5)
  # Per-row and helper arguments are columns, not settings of the call:
  args |>
    names() |>
    intersect(c("digits_x", "items_averaged")) |>
    expect_length(0L)
  # Settings survive verbs that keep the class:
  pigs2 |>
    grim_map(digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |>
    dplyr::filter(consistency) |>
    scrutiny_args() |>
    getElement("x_meaning") |>
    expect_equal("percentage")
})


test_that("a mapper called on a 0-row data frame returns a valid tibble", {
  for (out in list(
    pigs1[0L, ] |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer"),
    pigs5[0L, ] |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer"),
    pigs3[0L, ] |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer")
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

  # A required argument is still required:
  pigs1[0L, ] |> grim_map(data_type = "integer")             |> expect_error("digits_x")
  pigs1[0L, ] |> grim_map(digits_x = 2, data_type = "integer") |> expect_error("items_averaged")
  pigs1[0L, ] |> grim_map_seq(data_type = "integer")         |> expect_error("digits_x")
})


test_that("`data` must be a data frame, and that is checked first of all", {
  # Anything other than a data frame is rejected before the mapper reads a
  # single column name. Only the wording of the error depends on what the
  # object turned out to be.
  for (mapper in list(
    function(d) grim_map(d, digits_x = 2, items_averaged = 1, data_type = "integer"),
    function(d) grimmer_map(d, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer"),
    function(d) debit_map(d, digits_x = 2, digits_sd = 2, data_type = "integer"),
    function(d) grim_map_seq(d, digits_x = 2, items_averaged = 1, data_type = "integer"),
    function(d) grim_map_total_n(d, digits_x = 2, items_averaged = 1, data_type = "integer")
  )) {
    # Not aligning pipes here because the lengths are too different
    pigs1 |> as.matrix() |> mapper() |> expect_error("must be a data frame")
    1:10 |> mapper() |> expect_error("must be a data frame")
    NULL |> mapper() |> expect_error("must be a data frame")
  }

  # Everything else is named for what it is:
  1:10 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("integer vector")

  # The most confusing case is an undefined `data`, which R resolves to
  # `utils::data()`. The calls below name that function explicitly, because a
  # bare `data` would find any `data` object in the global environment first
  # when the tests run interactively. This used to be reported as the key
  # columns missing from `data`, which blamed the user's data for what was
  # really the wrong object:
  utils::data |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("must be a data frame")
  utils::data |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("`data\\(\\)` function")
  utils::data |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    expect_error("`data\\(\\)` function")
  utils::data |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
    expect_error("`data\\(\\)` function")
  utils::data |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("`data\\(\\)` function")
  utils::data |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("`data\\(\\)` function")

  # A tibble that really is missing the key columns still gets the column
  # error, not the type error:
  tibble::tibble(a = 1, b = 2) |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    expect_error("must be in `data`")
})


test_that("an `n` too large for integer keeps its value", {
  # `as.integer()` turns anything beyond `.Machine$integer.max` into `NA` with
  # nothing but a base-R warning. The verdict is computed from the real `n`
  # before that, so the row came out saying `n = NA` and `consistency = TRUE`
  # at once -- while a missing `n` yields an `NA` verdict everywhere else.
  df <- tibble::tibble(x = c(5.19, 5.19), n = c(28, 3e9))
  out <- df |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_no_warning()
  out$n |> expect_equal(c(28, 3e9))
  out$n |> expect_type("double")
  out$consistency |> expect_equal(c(FALSE, TRUE))

  # Within integer range, `n` is still integer, which is what makes it print
  # without a decimal point:
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")     |> purrr::pluck("n") |>
    expect_type("integer")
  pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer") |> purrr::pluck("n") |>
    expect_type("integer")
  tibble::tibble(x1 = 4.52, x2 = 5.23, n = 40L) |>
    grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("n") |>
    expect_type("integer")

  # The seq and total-n tiers coerce their own `n` columns, and they need the
  # same guard:
  df_seq <- tibble::tibble(x = 5.19, n = 3e9)
  out_seq <- df_seq |>
    grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer", var = "x", include_consistent = TRUE) |>
    expect_no_warning()
  out_seq$n |> unique() |> expect_equal(3e9)
  tibble::tibble(x1 = 4.52, x2 = 5.23, n = 6e9) |>
    grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 0:1) |>
    expect_no_warning()
})


test_that("vector-valued `rounding` and `symmetric` are rejected clearly", {
  # These describe one rounding procedure, so `reround()` has always required
  # length 1. The mappers passed them straight down to the `*_scalar()`
  # function, where R's own errors surfaced instead ("no such index at level
  # 1", "'length = 2' in coercion to 'logical(1)'"). The check now sits in
  # `rounding_offsets()`, which every one of the three tests reaches:
  for (mapper in list(
    function(...) grim_map(pigs1, digits_x = 2, items_averaged = 1, data_type = "integer", ...),
    function(...) grimmer_map(pigs5, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", ...),
    function(...) debit_map(pigs3, digits_x = 2, digits_sd = 2, data_type = "integer", ...),
    function(...) grim_map_seq(pigs1, digits_x = 2, items_averaged = 1, data_type = "integer", ...),
    function(...) grim_map_total_n(
      tibble::tibble(x1 = 4.52, x2 = 5.23, n = 40L), digits_x = 2, items_averaged = 1, data_type = "integer", ...
    )
  )) {
    mapper(rounding = c("up", "down")) |> expect_error("must each have length")
    mapper(symmetric = c(TRUE, FALSE)) |> expect_error("must each have length")
  }

  # `reround()` and `unround()` are unaffected: the first has always had the
  # check, and the second is documented as vectorized over `rounding`.
  1.234 |> roundwork::reround(2, rounding = c("up", "down")) |>
    expect_error("must each have length")
  "1.2" |> roundwork::unround(rounding = c("up", "down")) |>
    nrow() |>
    expect_equal(2L)
})


test_that("the other settings must be length 1 in every tier", {
  # Not rlang's "must be one of ..., not \"se\"" for an enum, which used to
  # pass a congruence check that allowed one setting longer than 1:
  for (mapper in list(
    function(...) grimmer_map(pigs5, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", ...),
    function(...) debit_map(pigs3, digits_x = 2, digits_sd = 2, data_type = "integer", ...),
    function(...) grimmer_map_seq(pigs5, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", ...),
    function(...) grimmer_map_total_n(
      tibble::tibble(x1 = 4.52, x2 = 5.23, sd1 = 1.2, sd2 = 1.3, n = 40L),
      digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", ...
    )
  )) {
    mapper(sd_meaning = c("se", "sd_pop")) |> expect_error("`sd_meaning` must be length 1, not 2")
    mapper(sd_meaning = SD_MEANINGS) |> expect_no_error()
  }
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", x_meaning = c("percentage", "mean")) |>
    expect_error("`x_meaning` must be length 1, not 2")
})


test_that("a setting whose default is not a single value may stay that way", {
  # d540b95 required every setting to be length 1, which broke these:
  df <- tibble::tibble(y = 1, n = 2)
  bounds_map <- function_map(.fn = function(y, n, bounds = NULL) TRUE, .reported = c("y", "n"), .name_test = "Bounds")
  scale_map  <- function_map(.fn = function(y, n, scale = c(1, 7)) TRUE, .reported = c("y", "n"), .name_test = "Scale")
  df |> bounds_map()                   |> _$consistency |> expect_true()
  df |> scale_map()                    |> _$consistency |> expect_true()
  df |> scale_map(scale = c(1, 5))     |> _$consistency |> expect_true()
})


test_that("integer `n` and `digits_*` columns are rounded, not truncated", {
  # `100 * 0.29` is 28.999999999999996, which was shown as 28 next to the
  # verdict for 29:
  tibble::tibble(x = 5.31, n = 100 * 0.29) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> _$n |> expect_identical(29L)
  # A `digits_x` just below 2 was shown as 1, and the seq mapper's re-tests
  # then found the column and the argument at odds:
  tibble::tibble(x = 5.19, n = 28) |> grim_map(digits_x = 2 - 1e-9, items_averaged = 1, data_type = "integer")     |> _$digits_x |> expect_identical(2L)
  tibble::tibble(x = 5.19, n = 40) |> grim_map_seq(digits_x = 2 - 1e-9, items_averaged = 1, data_type = "integer") |> _$digits_x |> unique() |> expect_identical(2L)

  # Shown as 2 but tested as 1.999999999, which flipped GRIM's verdict under
  # `"ceiling"` and left GRIMMER undecidable; the total-n mapper split a
  # near-whole total as odd, into 19.5 and 20.5:
  tibble::tibble(x = 1.5, n = 2 - 1e-9)                   |> grim_map(digits_x = 1, items_averaged = 1, data_type = "integer", rounding = "ceiling")   |> _$consistency |> expect_true()
  tibble::tibble(x = 1.5, n = 2, items_averaged = 1 - 1e-9) |> grim_map(digits_x = 1, rounding = "ceiling", data_type = "integer")                      |> _$consistency |> expect_true()
  tibble::tibble(x = 1.5, n = 2, items_averaged = 1 - 1e-9) |> grim_map(digits_x = 1, rounding = "ceiling", data_type = "integer")                      |> _$items_averaged |> expect_identical(1L)
  tibble::tibble(absolute = 100 * 0.29, percentage = 72.5, n = 40) |> is_percentage_of_map(digits_percentage = 1) |> _$absolute |> expect_identical(29L)
  tibble::tibble(x = 1.5, sd = 0.71, n = 2 - 1e-9)        |> grimmer_map(digits_x = 1, digits_sd = 2, items_averaged = 1, data_type = "integer")       |> _$consistency |> expect_true()
  near_whole <- tibble::tibble(x1 = 5.19, x2 = 4.5, n = 40 - 1e-9) |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer") |> unclass_scr()
  whole      <- tibble::tibble(x1 = 5.19, x2 = 4.5, n = 40)        |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer") |> unclass_scr()
  near_whole |> expect_equal(whole)
})


test_that("a by-row argument that is no decimal count is not made integer", {
  toy_map <- function_map(
    .fn = function(x, n, tol) abs(x - n) <= tol,
    .reported = c("x", "n"),
    .name_test = "toy",
    .args_by_row = "tol"
  )
  # `1e-8` is within tolerance of 0, and became 0L:
  tibble::tibble(x = 1, n = 2) |> toy_map(tol = 1e-8) |> _$tol |> expect_identical(1e-8)
})


test_that("a by-row argument that is `NULL` or a string is handled", {
  toy_map <- function_map(
    .fn = function(y, n, tol = NULL, method = "a") abs(y - n) <= (if (is.null(tol)) 0 else tol),
    .reported = c("y", "n"),
    .name_test = "toy",
    .args_by_row = c("tol", "method")
  )
  data <- tibble::tibble(y = 1:3, n = c(1, 3, 5))
  # A `NULL` default made a column named `NA`:
  data |> toy_map() |> colnames() |> expect_equal(c("y", "n", "method", "consistency"))
  # Strings that disagree both coerced to `NA`, which agreed:
  data |> dplyr::mutate(method = "b") |> toy_map(method = "a") |> expect_error("the two disagree")
  data |> dplyr::mutate(method = "a") |> toy_map(method = "a") |> _$method |> expect_equal(rep("a", 3))
})


test_that("a mapper's argument errors are not wrapped in `pmap()` context", {
  # Anything wrong with the arguments themselves fails on the first row just as
  # it would on any other, so the mapper applies the test to that row on its
  # own first. `purrr::pmap()`'s "i In index: 1." context would only obscure
  # the message. This used to happen for a missing required argument but not
  # for a bad `rounding` string:
  err <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "nonsense") |> tryCatch_error()
  err |> expect_s3_class("error")
  msg <- error_message_full(err)
  msg |> expect_match("one of the rounding methods")
  msg |> grepl(pattern = "In index", fixed = TRUE) |> expect_false()

  # Same for the missing-argument case, which is what the pre-application was
  # originally added for:
  err <- pigs1 |> grim_map(data_type = "integer") |> tryCatch_error()
  msg <- error_message_full(err)
  msg |> expect_match("digits_x")
  msg |> grepl(pattern = "In index", fixed = TRUE) |> expect_false()
})


# `absorb_key_args()` used to read the key arguments off the unevaluated call,
# where `lapply()` shows `FUN(X[[i]], ...)` and a wrapper shows a variable name.
test_that("key arguments work when the mapper is called indirectly", {
  d <- tibble::tibble(mean = c(5.19, 5.2), n = c(28, 30))
  expected <- d |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", x = mean)
  list(d) |> lapply(grim_map, digits_x = 2, items_averaged = 1, data_type = "integer", x = "mean")     |> purrr::pluck(1L) |> expect_equal(expected)
  list(d) |> purrr::map(grim_map, digits_x = 2, items_averaged = 1, data_type = "integer", x = "mean") |> purrr::pluck(1L) |> expect_equal(expected)
  wrapper <- function(data, col) grim_map(data, digits_x = 2, items_averaged = 1, data_type = "integer", x = col)
  d |> wrapper("mean") |> expect_equal(expected)
  d |>
    grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer", x = mean) |>
    purrr::pluck("x", 1L) |>
    expect_equal(5.14)
})

# A wrapper forwarding its own `NULL` default used to have the variable's name
# read as a column name.
test_that("a key argument forwarded as `NULL` is left out", {
  d <- tibble::tibble(x = c(5.19, 5.2), n = c(28, 30))
  wrapper     <- function(data, col = NULL) grim_map(data, digits_x = 2, items_averaged = 1, data_type = "integer", x = col)
  wrapper_seq <- function(data, col = NULL) grim_map_seq(data, digits_x = 2, items_averaged = 1, data_type = "integer", x = col)
  d |> wrapper()     |> expect_equal(grim_map(d, digits_x = 2, items_averaged = 1, data_type = "integer"))
  d |> wrapper_seq() |> expect_equal(grim_map_seq(d, digits_x = 2, items_averaged = 1, data_type = "integer"))
  d |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", x = if (FALSE) "y") |> expect_equal(grim_map(d, digits_x = 2, items_averaged = 1, data_type = "integer"))
})

# It also ignored a key argument whenever `data` had a column of that name, and
# tested that column instead.
test_that("`absorb_key_args()` reads key arguments passed through dots", {
  df <- pigs1 |> dplyr::rename(mean = x)
  my_map <- function(data, ...) absorb_key_args(data, c("x", "n"))
  df |> my_map(x = "mean") |> colnames() |> expect_equal(c("x", "n"))
  df |> my_map(x = mean)   |> colnames() |> expect_equal(c("x", "n"))
  # At the top level, there is no calling function to read formals from:
  pigs1 |> absorb_key_args(c("x", "n")) |> expect_no_warning()
})


test_that("a key argument clashing with an existing column is an error", {
  d <- tibble::tibble(x = c(1.11, 2.22), mean = c(5.19, 5.2), n = c(28, 30))
  d |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", x = mean) |> expect_error("already has a `x` column")
  d |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", x = x)    |> expect_equal(grim_map(d, digits_x = 2, items_averaged = 1, data_type = "integer"))
})


# `write_code_col_key_result()` renamed a `consistency` column that
# `.col_names` had already named otherwise, so the mapper always failed.
test_that("`.col_names` may start with a custom `.name_key_result`", {
  fn <- function(y, n, show_rec = FALSE) if (show_rec) list(y / 3 > n, y / 3) else y / 3 > n
  map_verdict <- function_map(.fn = fn, .reported = c("y", "n"), .name_test = "T", .name_key_result = "verdict", .col_names = c("verdict", "third"))
  df <- tibble::tibble(y = 16:18, n = 3:5)
  df      |> map_verdict()                |> colnames() |> expect_equal(c("y", "n", "verdict"))
  df      |> map_verdict(show_rec = TRUE) |> colnames() |> expect_equal(c("y", "n", "verdict", "third"))
  df[0, ] |> map_verdict(show_rec = TRUE) |> colnames() |> expect_equal(c("y", "n", "verdict", "third"))
  fn |>
    function_map(.reported = c("y", "n"), .name_test = "T", .name_key_result = "verdict", .col_names = c("consistency", "third")) |>
    expect_error("must start with `.name_key_result`")
})


test_that("`function_map()` rejects arguments it can't honor", {
  fn <- function(y, n, z = 1) y > n
  fn |>
    function_map(.reported = c("y", "n"), .name_test = "T", .args_fixed = list(n = 2)) |>
    expect_error("key, by-row")
  fn |>
    function_map(.reported = c("y", "n"), .name_test = "T", .cols_helper = "z", .args_fixed = list(z = 2)) |>
    expect_error("`z` is already one")
  fn |>
    function_map(.reported = c("y", "n"), .name_test = "T", .args_fixed = list(2)) |>
    expect_error("must be a named list")
  fn_shadowed <- function(y, n, fn = 1) y > n
  fn_shadowed |> function_map(.reported = c("y", "n"), .name_test = "T") |> expect_error("Rename `fn`")
  # These were assigned before the arguments were read, but not reserved:
  fn_enum <- function(y, n, enum_vals = 0) y > n
  fn_enum |> function_map(.reported = c("y", "n"), .name_test = "T") |> expect_error("Rename `enum_vals`")
})


# Only the other arguments of `.fn` were checked. A key column named `case` got
# the case numbers in place of its dispersed values, and so on.
test_that("the factories refuse key columns named like their own names", {
  map_named <- function(name) {
    fn <- rlang::new_function(setNames(alist(a = , n = ), c(name, "n")), quote(TRUE))
    function_map(.fn = fn, .reported = c(name, "n"), .name_test = "T")
  }
  for (name in c("data", "fn", "reported")) {
    name |> map_named() |> expect_error("uses internally")
  }

  for (name in c("case", "diff_var", "var", "dispersion")) {
    name |>
      map_named() |>
      function_map_seq(.reported = c(name, "n"), .name_test = "T") |>
      expect_error("`function_map_seq\\(\\)` uses internally")
  }

  for (name in c("case", "dir", "n_change", "times", "df_list")) {
    name |>
      map_named() |>
      function_map_total_n(.reported = name, .name_test = "T") |>
      expect_error("`function_map_total_n\\(\\)` uses internally")
  }

  # Any other name works in all three:
  map_score <- map_named("score")
  map_score |> function_map_seq(.reported = c("score", "n"), .name_test = "T") |> expect_no_error()
  map_score |> function_map_total_n(.reported = "score", .name_test = "T")     |> expect_no_error()
})


# `check_args_disabled()` read the names off the call, which through `lapply()`
# is `FUN(X[[i]], ...)`, so a disabled argument went through.
test_that("disabled arguments stay disabled when the mapper is called indirectly", {
  fn <- function(y, n, bad = 1) (y / 3) > n + bad
  map_disabled <- function_map(.fn = fn, .reported = c("y", "n"), .name_test = "T", .args_disabled = "bad")
  df <- tibble::tibble(y = 16:18, n = 3:5)
  df        |> map_disabled(bad = 100)            |> expect_error("disabled")
  list(df)  |> lapply(map_disabled, bad = 100)     |> expect_error("disabled")
  list(df)  |> purrr::map(map_disabled, bad = 100) |> expect_error("disabled")
})


# Such a column was silently dropped in favor of the result column.
test_that("a data column named like a result column is an error", {
  pigs1 |> dplyr::mutate(exp_incon = 1) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")                   |> expect_error("`exp_incon` would be overwritten")
  pigs5 |> dplyr::mutate(reason = "a")    |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_error("`reason` would be overwritten")
})


test_that("a `.cols_derived` function must return one value per row", {
  # A wrong length used to make a tibble that only failed when printed:
  bad <- function_map(
    .fn = function(x, n) x > n,
    .reported = c("x", "n"),
    .name_test = "T",
    .cols_derived = list(twice = function(x) c(x, x))
  )
  tibble::tibble(x = 1:2, n = 0:1) |> bad() |> expect_error("one value per row")
})

test_that("`.fn` must return a single logical value", {
  nul <- function_map(.fn = function(x, n) NULL, .reported = c("x", "n"), .name_test = "T")
  tibble::tibble(x = 1:2, n = 0:1) |> nul() |> expect_error("single logical value")
})

test_that("a `NULL` from `.fn` in a later row is an error, not another row's result", {
  # `unlist()` dropped the `NULL`s, and the first row's `TRUE` was recycled:
  later <- function_map(.fn = function(x, n) if (x > 1) NULL else TRUE, .reported = c("x", "n"), .name_test = "T")
  tibble::tibble(x = 1:3, n = 1) |> later() |> expect_error("returned no value for 2 rows")
  tibble::tibble(x = c(1, 1), n = 1) |> later() |> _$consistency |> expect_equal(c(TRUE, TRUE))
})

test_that("several values from `.fn` without `.col_names` are an error", {
  # Both failed with a raw tibble error:
  later <- function_map(.fn = function(x, n) if (x > 1) c(TRUE, FALSE) else TRUE, .reported = c("x", "n"), .name_test = "T")
  pairs <- function_map(.fn = function(x, n) list(TRUE, 1),                      .reported = c("x", "n"), .name_test = "T")
  tibble::tibble(x = 1:2, n = 3) |> later() |> expect_error("returned 2 values for at least one row")
  tibble::tibble(x = 1:2, n = 3) |> pairs() |> expect_error("returned 2 values for at least one row")
})

test_that("`.name_key_result` is any single string", {
  # A space failed in `parse()`, and two strings in a coercion:
  spaced <- function_map(.fn = function(x, n) TRUE, .reported = c("x", "n"), .name_test = "T", .name_key_result = "my result")
  tibble::tibble(x = 1, n = 2) |> spaced() |> _$`my result` |> expect_true()
  function_map(.fn = function(x, n) TRUE, .reported = c("x", "n"), .name_test = "T", .name_key_result = c("a", "b")) |> expect_error("single string")
})

test_that("a variadic column can't be renamed to a name the output uses", {
  tibble::tibble(a = 1:2, b = 3:4, total = 4:5) |>
    add_up_to_map(summands = c(total = a, b)) |>
    expect_error("`total` twice")
  # Another column of `data` used to be dropped in favor of the renamed one:
  tibble::tibble(a = 1:2, b = 3:4, total = 4:5, id = c("p", "q")) |>
    add_up_to_map(summands = c(id = a, b)) |>
    expect_error("`id` twice")
})

test_that("`.args_disabled` covers formals of the sequence mapper", {
  seq_disabled <- function_map_seq(
    .fn = grim_map,
    .reported = c("x", "n"),
    .name_test = "GRIM",
    .args_disabled = "dispersion"
  )
  pigs1 |> seq_disabled(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 1:2) |> expect_error("disabled")
  pigs1 |> seq_disabled(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_no_error()
})

test_that("a by-row argument with a default takes its values from a column", {
  # The default used to be compared to the column and to fail as disagreeing:
  dflt <- function_map(
    .fn = function(x, n, digits_y = 2) x > n + digits_y,
    .reported = c("x", "n"),
    .name_test = "T",
    .args_by_row = "digits_y"
  )
  out <- tibble::tibble(x = c(5, 5), n = c(1, 1), digits_y = c(1, 5)) |> dflt()
  out$consistency |> expect_equal(c(TRUE, FALSE))
  out$digits_y |> expect_equal(c(1, 5))
})

test_that("a space in `.name_test` becomes an underscore in the class", {
  spaced <- function_map(.fn = function(x, n) x > n, .reported = c("x", "n"), .name_test = "My Test")
  tibble::tibble(x = 1, n = 0) |> spaced() |> class() |> purrr::pluck(1) |> expect_equal("scrutiny_my_test_map")
})


test_that("a `data.frame` is coerced to a tibble, keeping its row names", {
  # Tables from `read.csv()` or a PDF extractor are data frames, so rejecting
  # them hit the main workflow on the first call. The result is the same as
  # for the tibble, and the output is a tibble:
  df <- as.data.frame(pigs1)
  from_tibble <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  from_df     <- df    |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  from_df |> expect_equal(from_tibble)
  df |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    expect_equal(grim_map_seq(pigs1, digits_x = 2, items_averaged = 1, data_type = "integer"))

  # A tibble has no row names, so character ones would be lost in silence.
  # They are returned as a column instead, like any other column of `data`:
  rownames(df) <- paste0("study_", seq_len(nrow(df)))
  out <- df |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  out$rowname |> expect_equal(rownames(df))
  out |> dplyr::select(-rowname) |> expect_equal(from_tibble)

  # Integer row names, as left by `df[rows, ]`, are row numbers, not labels:
  df <- as.data.frame(pigs1)
  df[df$n > 25, ] |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> rlang::has_name("rowname") |> expect_false()

  # A `rowname` column next to row names would appear twice:
  df_labeled <- data.frame(x = 5.19, n = 28, rowname = "a", row.names = "study_1")
  df_labeled |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("has row names and a `rowname` column")

  # A data frame or tibble with duplicate column names, as from `read.csv()`
  # with `check.names = FALSE`, is an error in every tier:
  df_dup <- data.frame(x = 1, n = 2, check.names = FALSE)
  names(df_dup) <- c("x", "x")
  df_dup |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("more than one column named `x`")
  tbl_dup <- tibble::tibble(x = 5.19, n = 28, z = 1, z = 2, .name_repair = "minimal")
  tbl_dup |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")     |> expect_error("more than one column named `z`")
  tbl_dup |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("more than one column named `z`")
  df_unnamed <- data.frame(5.19, 28, 1, 2)
  names(df_unnamed) <- c("x", "n", "", NA)
  df_unnamed |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("Columns 3, 4 have none")

  # A helper argument of the wrong length is the mapper's error, not dplyr's:
  tibble::tibble(x = c(5.19, 4.56, 7.1), n = c(28, 30, 15)) |> grim_map(digits_x = 2, items_averaged = c(1, 2), data_type = "integer") |> expect_error("`items_averaged` must have length 1 or the number of rows")
})


test_that("an error in a later row names that row, not purrr's index", {
  tibble::tibble(x = c(5.19, 5.123), n = c(28, 30)) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("In row 2 of `data`", fixed = TRUE)
  tibble::tibble(x = c(5.19, 5.123), n = c(28, 30)) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error(class = "rlang_error") |> class() |> expect_no_match("purrr_error_indexed")
})

test_that("an error in row 1 names it only if it is about that row", {
  # Row 2 passes, so the error is about row 1:
  tibble::tibble(x = c(5.123, 5.19), n = c(30, 28)) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("In row 1 of `data`", fixed = TRUE)
  # An error about the arguments fails on every row:
  df <- tibble::tibble(x = c(5.19, 5.18), n = 28)
  df |> grim_map(items_averaged = 1, data_type = "integer")                                    |> expect_error(class = "rlang_error") |> conditionMessage() |> expect_no_match("In row", fixed = TRUE)
  df |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "nope")   |> expect_error(class = "rlang_error") |> conditionMessage() |> expect_no_match("In row", fixed = TRUE)
})


# An enum given as its whole default set, e.g. by a wrapper forwarding its own
# default, was recorded as the set, and `grimmer_map_seq()` then failed on it:
test_that("an enum given as its whole default set is recorded as its first value", {
  df <- tibble::tibble(x = c(5.19, 4.5), sd = c(1.2, 2.1), n = c(28, 30), items_averaged = 1)
  df |> grimmer_map(digits_x = 2, digits_sd = 1, sd_meaning = SD_MEANINGS, data_type = "integer") |> scrutiny_args() |> _$sd_meaning |> expect_equal("sd_sample")
  df |> grimmer_map(digits_x = 2, digits_sd = 1, sd_meaning = SD_MEANINGS, data_type = "integer") |> unclass_scr() |> expect_equal(df |> grimmer_map(digits_x = 2, digits_sd = 1, data_type = "integer") |> unclass_scr())
  df |> grimmer_map_seq(digits_x = 2, digits_sd = 1, sd_meaning = SD_MEANINGS, data_type = "integer") |> unclass_scr() |> expect_equal(df |> grimmer_map_seq(digits_x = 2, digits_sd = 1, data_type = "integer") |> unclass_scr())
  df |> grimmer_map(digits_x = 2, digits_sd = 1, sd_meaning = c("se", "sd_pop"), data_type = "integer") |> expect_error("must be length 1")
})

test_that("a non-logical result in a later row is an error, not a character column", {
  mixed_scalar <- function(a, b) if (a == 2) NA_character_ else a > b
  mixed_map <- function_map(.fn = mixed_scalar, .reported = c("a", "b"), .name_test = "MIXED")
  tibble::tibble(a = c(1, 2), b = c(0, 0)) |> mixed_map() |> expect_error("for every row")
  tibble::tibble(a = c(1, 3), b = c(0, 0)) |> mixed_map() |> _$consistency |> expect_equal(c(TRUE, TRUE))
})

test_that("a `NULL` helper argument adds no column", {
  helper_null <- function(x, n, w = NULL) TRUE
  helper_null_map <- function_map(.fn = helper_null, .reported = c("x", "n"), .name_test = "H", .cols_helper = "w")
  tibble::tibble(x = 1:2, n = 3:4) |> helper_null_map() |> colnames() |> expect_equal(c("x", "n", "consistency"))
  tibble::tibble(x = 5.19, n = 28) |> grim_map(digits_x = 2, items_averaged = NULL, data_type = "integer") |> expect_error("Need to specify `items_averaged`")
  tibble::tibble(x1 = 3.43, x2 = 5.28, n = 90) |> grim_map_total_n(digits_x = 2, items_averaged = NULL, data_type = "integer") |> expect_error("Need to specify `items_averaged`")
})


test_that("factor columns from `read.csv(stringsAsFactors = TRUE)` get their own errors", {
  # A factor `data_type` failed with rlang's generic type error:
  data.frame(x = c(5.19, 5.18), n = 28, data_type = c("integer", "continuous"), stringsAsFactors = TRUE) |>
    grim_map(digits_x = 2, items_averaged = 1) |>
    expect_error("Convert it with `as.character\\(\\)`")
  # A factor `x` got the message about defunct string columns:
  data.frame(x = c("5.19", "5.18"), n = 28, stringsAsFactors = TRUE) |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    expect_error("must be numeric, not a factor")
  # Also in a total-n mapper, which splits rows into strings:
  data.frame(x1 = "5.19", x2 = 4.2, n = 40, stringsAsFactors = TRUE) |>
    grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    expect_error("`x1` must be numeric, not a factor")
  # The hint used to name `x` in its code, although the column is `m`:
  data.frame(m = "5.19", n = 28, stringsAsFactors = TRUE) |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", x = m) |>
    expect_error("as.numeric(as.character())", fixed = TRUE)
})


test_that("0-row output has the columns of output with rows", {
  # Only the key result column was created, as no result showed the others:
  grimmer_map(pigs5[0, ], digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")   |> colnames() |> expect_equal(colnames(grimmer_map(pigs5, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")))
  grim_map(pigs1[0, ], digits_x = 2, items_averaged = 1, data_type = "integer", show_rec = TRUE)      |> colnames() |> expect_equal(colnames(grim_map(pigs1, digits_x = 2, items_averaged = 1, data_type = "integer", show_rec = TRUE)))
  debit_map(pigs3[0, ], digits_x = 3, digits_sd = 3, data_type = "integer", show_rec = TRUE)          |> colnames() |> expect_equal(colnames(debit_map(pigs3, digits_x = 3, digits_sd = 3, data_type = "integer", show_rec = TRUE)))
  # A test that fails on missing values still gives the key result column:
  fn <- function(y, n, show_rec = FALSE) if (anyNA(c(y, n))) stop("no NA") else if (show_rec) list(y > n, y) else y > n
  map_strict <- function_map(.fn = fn, .reported = c("y", "n"), .name_test = "T", .col_names = c("consistency", "y_rec"))
  tibble::tibble(y = 1:2, n = 0:1)[0, ] |> map_strict(show_rec = TRUE) |> colnames() |> expect_equal(c("y", "n", "consistency"))  # What it says about missing values is not passed on:
  fn_loud <- function(x, n) { if (is.na(x)) { warning("x is NA"); message("x is NA") }; TRUE }
  map_loud <- function_map(.fn = fn_loud, .reported = c("x", "n"), .name_test = "T")
  tibble::tibble(x = numeric(), n = integer()) |> map_loud() |> expect_silent()
})


test_that("only a variadic mapper reserves the names of the variadic code", {
  # `index` became a reserved name for every mapper:
  function_map(.fn = function(index, n) TRUE, .reported = c("index", "n"), .name_test = "T") |> expect_no_error()
  function_map(.fn = function(x, n, index = 1) TRUE, .reported = c("x", "n"), .name_test = "T") |> expect_no_error()
})
