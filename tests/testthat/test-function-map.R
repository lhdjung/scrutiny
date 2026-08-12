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


test_that("the `digits_*` arguments come right after `data`", {
  # They have no defaults and must be given in every call, so they sit next to
  # the other argument that must, ahead of the key arguments -- and in the same
  # position in every mapper, basic and sequence alike:
  names(formals(grim_map))[1:2] |> expect_equal(c("data", "digits_x"))
  names(formals(grimmer_map))[1:3] |>
    expect_equal(c("data", "digits_x", "digits_sd"))
  names(formals(debit_map))[1:3] |>
    expect_equal(c("data", "digits_x", "digits_sd"))
  names(formals(grim_map_seq))[1:2] |> expect_equal(c("data", "digits_x"))
  names(formals(grimmer_map_seq))[1:3] |>
    expect_equal(c("data", "digits_x", "digits_sd"))
  names(formals(debit_map_seq))[1:3] |>
    expect_equal(c("data", "digits_x", "digits_sd"))
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


test_that("`.reported` may name any number of key columns", {
  # Nothing in the factory hardcodes the number of key columns: GRIM has two,
  # GRIMMER and DEBIT have three, and a test with more works the same way. The
  # number has to be known when the factory runs, not when the mapper is
  # called (#42).
  quadrant_scalar <- function(a, b, c, d, tolerance = 0) {
    abs((a + b) - (c + d)) <= tolerance
  }

  quadrant_map <- function_map(
    .fun = quadrant_scalar,
    .reported = c("a", "b", "c", "d"),
    .name_test = "QUADRANT"
  )

  names(formals(quadrant_map)) |>
    expect_equal(c("data", "a", "b", "c", "d", "tolerance", "..."))

  df <- tibble::tibble(a = 1:3, b = 4:6, c = c(5L, 7L, 9L), d = c(0L, 0L, 1L))
  out <- quadrant_map(df)
  out |> expect_s3_class("scrutiny_quadrant_map")
  out$consistency |> expect_equal(c(TRUE, TRUE, FALSE))
  out |> colnames() |> expect_equal(c("a", "b", "c", "d", "consistency"))

  # Key-column renaming covers all four of them:
  df_renamed <- dplyr::rename(df, alpha = a, delta = d)
  quadrant_map(df_renamed, a = alpha, d = delta) |> expect_equal(out)
  quadrant_map(df_renamed, a = alpha) |> expect_error()

  # And so does the arity-agnostic column check:
  quadrant_map(dplyr::select(df, -d)) |> expect_error()
})


test_that("`.reported_variadic` decides the number of key columns at call time", {
  # The other case of #42: a test over "any number of columns", where how many
  # there are is a property of the caller's data rather than of the factory
  # call. Its `.fun` takes them as one vector argument.
  sum_check_scalar <- function(parts, total, tolerance = 0) {
    abs(sum(parts) - total) <= tolerance
  }

  sum_check_map <- function_map(
    .fun = sum_check_scalar,
    .reported = "total",
    .reported_variadic = "parts",
    .name_test = "SUMCHECK"
  )

  # The variadic argument comes ahead of the fixed key arguments, and it has no
  # default, unlike them:
  names(formals(sum_check_map)) |>
    expect_equal(c("data", "parts", "total", "tolerance", "..."))
  formals(sum_check_map)$parts |> rlang::is_missing() |> expect_true()

  df <- tibble::tibble(
    item_1 = c(10, 20, 30),
    item_2 = c(5, 5, 5),
    item_3 = c(1, 2, 3),
    total = c(16, 27, 40),
    note = c("a", "b", "c")
  )

  out <- sum_check_map(df, parts = c(item_1, item_2, item_3))
  out |> expect_s3_class("scrutiny_sumcheck_map")
  out$consistency |> expect_equal(c(TRUE, TRUE, FALSE))

  # The selected columns are returned as themselves, so the output is as
  # rectangular as any other mapper's:
  out |>
    colnames() |>
    expect_equal(c("item_1", "item_2", "item_3", "total", "consistency", "note"))

  # Any tidyselect expression will do, and the number of columns it picks is
  # the number of values that each test gets:
  sum_check_map(df, parts = starts_with("item")) |>
    expect_equal(out)
  sum_check_map(df, parts = c(item_1, item_2))$consistency |>
    expect_equal(c(FALSE, FALSE, FALSE))
  sum_check_map(df, parts = c(item_1, item_2), tolerance = 100)$consistency |>
    expect_equal(c(TRUE, TRUE, TRUE))

  # A column that the selection leaves out is an ordinary other column:
  sum_check_map(df, parts = c(item_1, item_2)) |>
    colnames() |>
    expect_contains("item_3")

  # The fixed key column still supports renaming, and 0 rows still work:
  df_renamed <- dplyr::rename(df, sum_col = total)
  sum_check_map(df_renamed, parts = starts_with("item"), total = sum_col) |>
    expect_equal(out)
  sum_check_map(df[0L, ], parts = starts_with("item")) |>
    nrow() |>
    expect_equal(0L)
})


test_that("a variadic key argument must be specified, and must select real columns", {
  sum_check_scalar <- function(parts, total) sum(parts) == total
  sum_check_map <- function_map(
    .fun = sum_check_scalar,
    .reported = "total",
    .reported_variadic = "parts",
    .name_test = "SUMCHECK"
  )
  df <- tibble::tibble(a = 1, b = 2, total = 3)

  # No default: guessing the columns would quietly test the wrong ones.
  sum_check_map(df) |> expect_error("must be specified")
  sum_check_map(df, parts = c(a, nonexistent)) |> expect_error("doesn't exist")

  # An empty selection leaves the test function with no values. Without this
  # check, `purrr::pmap()` reports a recycling failure over a variable the
  # caller has never heard of:
  sum_check_map(df, parts = c()) |> expect_error("selected no columns")

  # A column that has a role already cannot also be tested as one of many
  # values: it would appear twice in the output, giving a tibble with
  # duplicate column names.
  sum_check_map(df, parts = c(a, total)) |> expect_error("role in the test")
  sum_check_map(df, parts = everything()) |> expect_error("role in the test")
  sum_check_map(df, parts = !total) |> expect_no_error()

  # Helper columns count as spoken for, too:
  helper_scalar <- function(parts, total, items = 1) sum(parts) == total * items
  helper_map <- function_map(
    .fun = helper_scalar,
    .reported = "total",
    .reported_variadic = "parts",
    .name_test = "SUMCHECK",
    .cols_helper = "items"
  )
  df_items <- tibble::tibble(a = 1, b = 2, total = 3, items = 1)
  helper_map(df_items, parts = everything()) |> expect_error("role in the test")
  helper_map(df_items, parts = c(a, b))$consistency |> expect_true()
})


test_that("`.reported` may be empty if `.reported_variadic` is not", {
  all_equal_scalar <- function(values) length(unique(values)) == 1L
  all_equal_map <- function_map(
    .fun = all_equal_scalar,
    .reported_variadic = "values",
    .name_test = "ALLEQUAL"
  )
  names(formals(all_equal_map)) |> expect_equal(c("data", "values", "..."))

  df <- tibble::tibble(a = c(1, 2), b = c(1, 3), c = c(1, 3), id = c("x", "y"))
  out <- all_equal_map(df, values = c(a, b, c))
  out$consistency |> expect_equal(c(TRUE, FALSE))
  out |> colnames() |> expect_equal(c("a", "b", "c", "consistency", "id"))

  # But one of the two must be there:
  function_map(.fun = all_equal_scalar, .name_test = "ALLEQUAL") |>
    expect_error("at least one column")
})


test_that("`.reported_variadic` composes with the factory's other arguments", {
  sum_check_scalar <- function(parts, total, digits_total, show_rec = FALSE) {
    gap <- sum(parts) - total
    consistency <- abs(gap) < 10^-digits_total
    if (show_rec) list(consistency, sum(parts), gap) else consistency
  }
  count_parts <- function(parts) length(parts)

  sum_check_map <- function_map(
    .fun = sum_check_scalar,
    .reported = "total",
    .reported_variadic = "parts",
    .name_test = "SUMCHECK",
    .args_by_row = "digits_total",
    .args_defaults = list(show_rec = TRUE),
    .col_names = c("consistency", "rec_sum", "gap"),
    .cols_derived = list(n_parts = count_parts)
  )

  # The by-row arguments still come first, ahead of the key arguments:
  names(formals(sum_check_map))[1:4] |>
    expect_equal(c("data", "digits_total", "parts", "total"))

  df <- tibble::tibble(
    i1 = c(1.5, 2.5),
    i2 = c(2.5, 2.5),
    total = c(4.0, 5.5)
  )
  out <- sum_check_map(df, digits_total = 1, parts = c(i1, i2))
  out |>
    colnames() |>
    expect_equal(c(
      "i1", "i2", "total", "digits_total",
      "consistency", "n_parts", "rec_sum", "gap"
    ))
  out$consistency |> expect_equal(c(TRUE, FALSE))
  out$rec_sum |> expect_equal(c(4, 5))
  # `.cols_derived` gets the row's values as one vector, like the test itself:
  out$n_parts |> expect_equal(c(2L, 2L))
  sum_check_map(df, digits_total = c(1, 2), parts = c(i1, i2))$digits_total |>
    expect_equal(c(1, 2))
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

  function_map(
    .fun = grim_scalar,
    .reported = c("x", "n"),
    .name_test = "GRIM",
    .reported_variadic = "values"
  ) |>
    expect_error()

  # A key argument takes either one column's values or those of any number of
  # columns, not both:
  function_map(
    .fun = grim_scalar,
    .reported = c("x", "n"),
    .name_test = "GRIM",
    .reported_variadic = "x"
  ) |>
    expect_error("must not also be")
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
    # `suppressMessages()` mutes the hint that `audit.scrutiny_grimmer_map()`
    # prints when there is no `reason` column, as here with `show_reason` unset:
    suppressMessages(audit(out)) |> nrow() |> expect_equal(1L)
  }
})


test_that("`data` must be a tibble, and that is checked first of all", {
  # Anything other than a tibble is rejected before the mapper reads a single
  # column name -- including a `data.frame`, which is never admitted even for
  # the length of one more check. Only the wording of the error depends on what
  # the object turned out to be.
  for (mapper in list(
    function(d) grim_map(d, digits_x = 2),
    function(d) grimmer_map(d, digits_x = 2, digits_sd = 2),
    function(d) debit_map(d, digits_x = 2, digits_sd = 2),
    function(d) grim_map_seq(d, digits_x = 2),
    function(d) grim_map_total_n(d, digits_x = 2)
  )) {
    mapper(as.data.frame(pigs1)) |> expect_error("must be a tibble")
    mapper(as.matrix(pigs1)) |> expect_error("must be a tibble")
    mapper(1:10) |> expect_error("must be a tibble")
    mapper(NULL) |> expect_error("must be a tibble")
  }

  # A `data.frame` gets the conversion hint...
  grim_map(as.data.frame(pigs1), digits_x = 2) |>
    expect_error("as_tibble")
  # ...and everything else is named for what it is:
  grim_map(1:10, digits_x = 2) |> expect_error("integer vector")

  # The most confusing case is an undefined `data`, which R resolves to
  # `utils::data()`. Nothing in this file defines a `data` object, so the calls
  # below really do hit that function. This used to be reported as the key
  # columns missing from `data`, which blamed the user's data for what was
  # really the wrong object:
  grim_map(data, digits_x = 2) |> expect_error("must be a tibble")
  grim_map(data, digits_x = 2) |> expect_error("`data\\(\\)` function")
  grimmer_map(data, digits_x = 2, digits_sd = 2) |>
    expect_error("`data\\(\\)` function")
  debit_map(data, digits_x = 2, digits_sd = 2) |>
    expect_error("`data\\(\\)` function")
  grim_map_seq(data, digits_x = 2) |> expect_error("`data\\(\\)` function")
  grim_map_total_n(data, digits_x = 2) |> expect_error("`data\\(\\)` function")

  # A tibble that really is missing the key columns still gets the column
  # error, not the type error:
  grim_map(tibble::tibble(a = 1, b = 2), digits_x = 2) |>
    expect_error("must be in `data`")
})
