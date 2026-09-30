# Manufactured functions --------------------------------------------------

# Stripped-down versions of the real mappers: same `.fun` and `.reported`, but
# none of the arguments that give the real ones their extra columns. They exist
# to check that the factory's core -- key columns, renaming, the key result
# column -- is what the real mappers get from it, and that the extras really are
# extras. The `"scrutiny"` attribute is ignored in the comparisons: it records
# the arguments that applied to the whole call, and these differ by design.

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
      "tbl_df",
      "tbl",
      "data.frame"
    )
  )


# Running old and new (= manufactured) functions --------------------------

out_grim_old1 <- df_grim1 |>
  grim_map(digits_x = 2) |>
  dplyr::select(x, n, consistency)
out_debit_old1 <- df_debit1 |>
  debit_map(digits_x = 2, digits_sd = 2) |>
  dplyr::select(x, sd, n, consistency)

out_grim_new1 <- grim_map_alt(df_grim1, digits_x = 2)
out_debit_new1 <- debit_map_alt(df_debit1, digits_x = 2, digits_sd = 2)

out_grim_old2 <- df_grim2 |>
  grim_map(digits_x = 2) |>
  dplyr::select(x, n, consistency)
out_debit_old2 <- df_debit2 |>
  debit_map(digits_x = 2, digits_sd = 2) |>
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
    .fun = grim_scalar,
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
    .fun = grimmer_scalar,
    .reported = c("x", "sd", "n"),
    .name_test = "GRIMMER"
  )
  df |> map_const(digits_x = 2, digits_sd = 2) |>
    colnames() |>
    expect_equal(c("x", "sd", "n", "consistency"))
  df |> map_const(digits_x = c(2, 1), digits_sd = 2) |> expect_error()

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
  pigs3 |> debit_map(digits_x = 2, digits_sd = 2, show_rec = FALSE) |>
    colnames() |>
    expect_equal(c("x", "sd", "n", "digits_x", "digits_sd", "consistency"))
})


test_that("`.cols_helper` supports helper columns", {
  df <- tibble::tibble(x = 4.67, sd = 0.00, n = 2L, items = 3)

  # `items` may be a column of `data`...
  df |>
    grimmer_map(digits_x = 2, digits_sd = 2) |>
    purrr::pluck("n") |>
    expect_equal(6L)
  # ...or an argument, but not both if they contradict each other:
  df |> grimmer_map(digits_x = 2, digits_sd = 2, items = 5) |> expect_error()
  pigs5 |>
    grimmer_map(digits_x = 2, digits_sd = 2, items = 2) |>
    purrr::pluck("n") |>
    expect_equal(as.integer(pigs5$n * 2))
})


test_that("`.args_defaults` overrides the test function's own defaults", {
  # `grimmer_scalar()` has `show_reason = FALSE`, `grimmer_map()` has `TRUE`:
  grimmer_scalar |> formals() |> call_on(\(x) x$show_reason) |> expect_false()
  grimmer_map    |> formals() |> call_on(\(x) x$show_reason) |> expect_true()
  pigs5 |>
    grimmer_map(digits_x = 2, digits_sd = 2) |>
    purrr::pluck("reason") |>
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
  map_disabled |> formals() |> names() |> expect_no_match("percent")
  pigs1 |> map_disabled(digits_x = 2, percent = TRUE) |> expect_error()
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
  # `digits_*` output column that `grim_plot()` reads:
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
    .fun = quadrant_scalar,
    .reported = c("a", "b", "c", "d"),
    .name_test = "QUADRANT"
  )

  quadrant_map |> formals() |> names() |>
    expect_equal(c("data", "a", "b", "c", "d", "tolerance", "..."))

  df <- tibble::tibble(a = 1:3, b = 4:6, c = c(5L, 7L, 9L), d = c(0L, 0L, 1L))
  out <- quadrant_map(df)
  out |> expect_s3_class("scrutiny_quadrant_map")
  out$consistency |> expect_equal(c(TRUE, TRUE, FALSE))
  out |> colnames() |> expect_equal(c("a", "b", "c", "d", "consistency"))

  # Key-column renaming covers all four of them:
  df_renamed <- dplyr::rename(df, alpha = a, delta = d)
  df_renamed |> quadrant_map(a = alpha, d = delta) |> expect_equal(out)
  df_renamed |> quadrant_map(a = alpha) |> expect_error()

  # And so does the arity-agnostic column check:
  df |> dplyr::select(-d) |> quadrant_map() |> expect_error()
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
  sum_check_map |> formals() |> names() |>
    expect_equal(c("data", "parts", "total", "tolerance", "..."))
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
  df_renamed <- dplyr::rename(df, sum_col = total)
  df_renamed |> sum_check_map(parts = starts_with("item"), total = sum_col) |>
    expect_equal(out)
  df[0L, ] |> sum_check_map(parts = starts_with("item")) |>
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
    .fun = helper_scalar,
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
    .fun = all_equal_scalar,
    .reported_variadic = "values",
    .name_test = "ALLEQUAL"
  )
  all_equal_map |> formals() |> names() |> expect_equal(c("data", "values", "..."))

  df <- tibble::tibble(a = c(1, 2), b = c(1, 3), c = c(1, 3), id = c("x", "y"))
  out <- all_equal_map(df, values = c(a, b, c))
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
      .cols_derived = list(probability = "grim_probability")
    ) |>
    expect_error()

  grim_scalar |>
    function_map(
      .reported = c("x", "n"),
      .name_test = "GRIM",
      .cols_derived = list(grim_probability)
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
  # `probability` comes from `grim_probability()`, not from `grim_scalar()`:
  out <- grim_map(pigs1, digits_x = 2)
  out$probability |> expect_equal(grim_probability(pigs1$x, pigs1$n, 2))
  # It follows the key result column, ahead of the `.col_names` columns:
  out |>
    colnames() |>
    expect_equal(c("x", "n", "digits_x", "consistency", "probability"))
  pigs1 |> grim_map(digits_x = 2, show_rec = TRUE) |>
    colnames() |>
    expect_equal(c(
      "x", "n", "digits_x", "consistency", "probability",
      "rec_sum", "sum_lower", "sum_upper", "rec_x_upper", "rec_x_lower"
    ))

  # The derived function only gets the arguments it has formals for, and
  # `rounding`, `items`, and `percent` must reach it:
  pigs1 |>
    grim_map(digits_x = 2, rounding = "ceiling") |>
    purrr::pluck("probability") |>
    expect_equal(grim_probability(pigs1$x, pigs1$n, 2, rounding = "ceiling"))
  pigs1 |>
    grim_map(digits_x = 2, items = 2) |>
    purrr::pluck("probability") |>
    expect_equal(grim_probability(pigs1$x, pigs1$n, 2, items = 2))
  pigs2 |>
    grim_map(digits_x = 1, percent = TRUE) |>
    purrr::pluck("probability") |>
    expect_equal(grim_probability(pigs2$x, pigs2$n, 1, percent = TRUE))
})


test_that("the arguments of the whole call are recorded, with defaults", {
  args <- pigs2 |>
    grim_map(digits_x = 1, percent = TRUE, rounding = "ceiling") |>
    scrutiny_meta() |>
    getElement("args")
  args$percent |> expect_true()
  args$rounding |> expect_equal("ceiling")
  args$threshold |> expect_equal(5)
  # Per-row and helper arguments are columns, not settings of the call, and a
  # missing value such as the deprecated `tolerance` is no setting at all:
  args |>
    names() |>
    intersect(c("digits_x", "items", "tolerance")) |>
    expect_length(0L)
  # Settings survive verbs that keep the class:
  pigs2 |>
    grim_map(digits_x = 1, percent = TRUE) |>
    dplyr::filter(consistency) |>
    scrutiny_meta() |>
    getElement("args") |>
    getElement("percent") |>
    expect_true()
})


test_that("a mapper called on a 0-row data frame returns a valid tibble", {
  for (out in list(
    pigs1[0L, ] |> grim_map(digits_x = 2),
    pigs5[0L, ] |> grimmer_map(digits_x = 2, digits_sd = 2),
    pigs3[0L, ] |> debit_map(digits_x = 2, digits_sd = 2)
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
    out |> audit() |> suppressMessages() |> nrow() |> expect_equal(1L)
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
    # Not aligning pipes here because the lengths are too different
    pigs1 |> as.data.frame() |> mapper() |> expect_error("must be a tibble")
    pigs1 |> as.matrix() |> mapper() |> expect_error("must be a tibble")
    1:10 |> mapper() |> expect_error("must be a tibble")
    NULL |> mapper() |> expect_error("must be a tibble")
  }

  # A `data.frame` gets the conversion hint...
  pigs1 |> as.data.frame() |> grim_map(digits_x = 2) |>
    expect_error("as_tibble")
  # ...and everything else is named for what it is:
  1:10 |> grim_map(digits_x = 2) |> expect_error("integer vector")

  # The most confusing case is an undefined `data`, which R resolves to
  # `utils::data()`. Nothing in this file defines a `data` object, so the calls
  # below really do hit that function. This used to be reported as the key
  # columns missing from `data`, which blamed the user's data for what was
  # really the wrong object:
  data |> grim_map(digits_x = 2) |> expect_error("must be a tibble")
  data |> grim_map(digits_x = 2) |> expect_error("`data\\(\\)` function")
  data |> grimmer_map(digits_x = 2, digits_sd = 2) |>
    expect_error("`data\\(\\)` function")
  data |> debit_map(digits_x = 2, digits_sd = 2) |>
    expect_error("`data\\(\\)` function")
  data |> grim_map_seq(digits_x = 2) |> expect_error("`data\\(\\)` function")
  data |> grim_map_total_n(digits_x = 2) |> expect_error("`data\\(\\)` function")

  # A tibble that really is missing the key columns still gets the column
  # error, not the type error:
  tibble::tibble(a = 1, b = 2) |>
    grim_map(digits_x = 2) |>
    expect_error("must be in `data`")
})


test_that("an `n` too large for integer keeps its value", {
  # `as.integer()` turns anything beyond `.Machine$integer.max` into `NA` with
  # nothing but a base-R warning. The verdict is computed from the real `n`
  # before that, so the row came out saying `n = NA` and `consistency = TRUE`
  # at once -- while a missing `n` yields an `NA` verdict everywhere else.
  df <- tibble::tibble(x = c(5.19, 5.19), n = c(28, 3e9))
  out <- df |> grim_map(digits_x = 2) |> expect_no_warning()
  out$n |> expect_equal(c(28, 3e9))
  out$n |> expect_type("double")
  out$consistency |> expect_equal(c(FALSE, TRUE))

  # Within integer range, `n` is still integer, which is what makes it print
  # without a decimal point:
  pigs1 |> grim_map(digits_x = 2)     |> purrr::pluck("n") |>
    expect_type("integer")
  pigs1 |> grim_map_seq(digits_x = 2) |> purrr::pluck("n") |>
    expect_type("integer")
  tibble::tibble(x1 = 4.52, x2 = 5.23, n = 40L) |>
    grim_map_total_n(digits_x = 2) |>
    purrr::pluck("n") |>
    expect_type("integer")

  # The seq and total-n tiers coerce their own `n` columns, and they need the
  # same guard:
  df_seq <- tibble::tibble(x = 5.19, n = 3e9)
  out_seq <- df_seq |>
    grim_map_seq(digits_x = 2, var = "x", include_consistent = TRUE) |>
    expect_no_warning()
  out_seq$n |> unique() |> expect_equal(3e9)
  tibble::tibble(x1 = 4.52, x2 = 5.23, n = 6e9) |>
    grim_map_total_n(digits_x = 2, dispersion = 0:1) |>
    expect_no_warning()
})


test_that("vector-valued `rounding` and `symmetric` are rejected clearly", {
  # These describe one rounding procedure, so `reround()` has always required
  # length 1. The mappers passed them straight down to the `*_scalar()`
  # function, where R's own errors surfaced instead ("no such index at level
  # 1", "'length = 2' in coercion to 'logical(1)'"). The check now sits in
  # `rounding_offsets()`, which every one of the three tests reaches:
  for (mapper in list(
    function(...) grim_map(pigs1, digits_x = 2, ...),
    function(...) grimmer_map(pigs5, digits_x = 2, digits_sd = 2, ...),
    function(...) debit_map(pigs3, digits_x = 2, digits_sd = 2, ...),
    function(...) grim_map_seq(pigs1, digits_x = 2, ...),
    function(...) grim_map_total_n(
      tibble::tibble(x1 = 4.52, x2 = 5.23, n = 40L), digits_x = 2, ...
    )
  )) {
    mapper(rounding = c("up", "down")) |> expect_error("must each have length")
    mapper(symmetric = c(TRUE, FALSE)) |> expect_error("must each have length")
  }

  # `reround()` and `unround()` are unaffected: the first has always had the
  # check, and the second is documented as vectorized over `rounding`.
  1.234 |> reround(2, rounding = c("up", "down")) |>
    expect_error("must each have length")
  "1.2" |> unround(rounding = c("up", "down")) |>
    nrow() |>
    expect_equal(2L)
})


test_that("a mapper's argument errors are not wrapped in `pmap()` context", {
  # Anything wrong with the arguments themselves fails on the first row just as
  # it would on any other, so the mapper applies the test to that row on its
  # own first. `purrr::pmap()`'s "i In index: 1." context would only obscure
  # the message. This used to happen for a missing required argument but not
  # for a bad `rounding` string:
  err <- pigs1 |> grim_map(digits_x = 2, rounding = "nonsense") |> tryCatch_error()
  expect_s3_class(err, "error")
  msg <- error_message_full(err)
  expect_match(msg, "designated string values")
  msg |> grepl(pattern = "In index", fixed = TRUE) |> expect_false()

  # Same for the missing-argument case, which is what the pre-application was
  # originally added for:
  err <- pigs1 |> grim_map() |> tryCatch_error()
  msg <- error_message_full(err)
  expect_match(msg, "digits_x")
  msg |> grepl(pattern = "In index", fixed = TRUE) |> expect_false()
})


# `absorb_key_args()` used to read the key arguments off the unevaluated call,
# where `lapply()` shows `FUN(X[[i]], ...)` and a wrapper shows a variable name.
test_that("key arguments work when the mapper is called indirectly", {
  d <- tibble::tibble(mean = c(5.19, 5.2), n = c(28, 30))
  expected <- grim_map(d, digits_x = 2, x = mean)
  expect_equal(lapply(list(d), grim_map, digits_x = 2, x = "mean")[[1L]], expected)
  expect_equal(purrr::map(list(d), grim_map, digits_x = 2, x = "mean")[[1L]], expected)
  wrapper <- function(data, col) grim_map(data, digits_x = 2, x = col)
  d |> wrapper("mean") |> expect_equal(expected)
  d |>
    grim_map_seq(digits_x = 2, x = mean) |>
    purrr::pluck("x", 1L) |>
    expect_equal(5.14)
})

# It also ignored a key argument whenever `data` had a column of that name, and
# tested that column instead.
test_that("`absorb_key_args()` reads key arguments passed through dots", {
  df <- dplyr::rename(pigs1, mean = x)
  my_map <- function(data, ...) absorb_key_args(data, c("x", "n"))
  df |> my_map(x = "mean") |> colnames() |> expect_equal(c("x", "n"))
  df |> my_map(x = mean)   |> colnames() |> expect_equal(c("x", "n"))
})


test_that("a key argument clashing with an existing column is an error", {
  d <- tibble::tibble(x = c(1.11, 2.22), mean = c(5.19, 5.2), n = c(28, 30))
  d |> grim_map(digits_x = 2, x = mean) |> expect_error("already has a `x` column")
  d |> grim_map(digits_x = 2, x = x)    |> expect_equal(grim_map(d, digits_x = 2))
})


# `write_code_col_key_result()` renamed a `consistency` column that
# `.col_names` had already named otherwise, so the mapper always failed.
test_that("`.col_names` may start with a custom `.name_key_result`", {
  fun <- function(y, n, show_rec = FALSE) if (show_rec) list(y / 3 > n, y / 3) else y / 3 > n
  map_verdict <- function_map(.fun = fun, .reported = c("y", "n"), .name_test = "T", .name_key_result = "verdict", .col_names = c("verdict", "third"))
  df <- tibble::tibble(y = 16:18, n = 3:5)
  df      |> map_verdict()                |> colnames() |> expect_equal(c("y", "n", "verdict"))
  df      |> map_verdict(show_rec = TRUE) |> colnames() |> expect_equal(c("y", "n", "verdict", "third"))
  df[0, ] |> map_verdict(show_rec = TRUE) |> colnames() |> expect_equal(c("y", "n", "verdict"))
  function_map(.fun = fun, .reported = c("y", "n"), .name_test = "T", .name_key_result = "verdict", .col_names = c("consistency", "third")) |>
    expect_error("must start with `.name_key_result`")
})


test_that("`function_map()` rejects arguments it can't honor", {
  fun <- function(y, n, z = 1) y > n
  function_map(.fun = fun, .reported = c("y", "n"), .name_test = "T", .args_defaults = list(n = 2)) |>
    expect_error("key or disabled argument")
  fun_shadowed <- function(y, n, fun = 1) y > n
  function_map(.fun = fun_shadowed, .reported = c("y", "n"), .name_test = "T") |>
    expect_error("Rename `fun`")
})


# `check_args_disabled()` read the names off the call, which through `lapply()`
# is `FUN(X[[i]], ...)`, so a disabled argument went through.
test_that("disabled arguments stay disabled when the mapper is called indirectly", {
  fun <- function(y, n, bad = 1) (y / 3) > n + bad
  map_disabled <- function_map(.fun = fun, .reported = c("y", "n"), .name_test = "T", .args_disabled = "bad")
  df <- tibble::tibble(y = 16:18, n = 3:5)
  df        |> map_disabled(bad = 100)            |> expect_error("disabled")
  list(df)  |> lapply(map_disabled, bad = 100)     |> expect_error("disabled")
  list(df)  |> purrr::map(map_disabled, bad = 100) |> expect_error("disabled")
})
