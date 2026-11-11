test_that("`manage_helper_col()` takes the helper from the column or the argument", {
  mapper <- function(data, items = 1) manage_helper_col(data, items, 1)
  mapper_no_col <- function(data, items = 1) manage_helper_col(data, items, 1, add_col = FALSE)

  tibble::tibble(x = 5.19)            |> mapper(items = 2)          |> expect_equal(tibble::tibble(x = 5.19, items = 2))
  tibble::tibble(x = 5.19)            |> mapper()                   |> expect_equal(tibble::tibble(x = 5.19, items = 1))
  tibble::tibble(x = 5.19)            |> mapper_no_col(items = 2)   |> expect_equal(tibble::tibble(x = 5.19))
  tibble::tibble(x = 5.19, items = 3) |> mapper()                   |> expect_equal(tibble::tibble(x = 5.19, items = 3))
  tibble::tibble(x = 5.19, items = 3) |> mapper(items = 2)          |> expect_error("already in")
})

test_that("`manage_helper_col()` without a default leaves a missing helper missing", {
  mapper <- function(data, items) manage_helper_col(data, items)

  tibble::tibble(x = 5.19)            |> mapper()          |> expect_equal(tibble::tibble(x = 5.19))
  tibble::tibble(x = 5.19)            |> mapper(items = 2) |> expect_equal(tibble::tibble(x = 5.19, items = 2))
  tibble::tibble(x = 5.19, items = 3) |> mapper()          |> expect_equal(tibble::tibble(x = 5.19, items = 3))
  # An argument that agrees with the column is no contradiction:
  tibble::tibble(x = 5.19, items = 3) |> mapper(items = 3) |> expect_equal(tibble::tibble(x = 5.19, items = 3))
  tibble::tibble(x = 5.19, items = 3) |> mapper(items = 4) |> expect_error("already in")
  # Only a single value or one per row can agree; others used to be cut or
  # recycled into agreement:
  tibble::tibble(x = 1:2, items = 1:2)    |> mapper(items = 1:3) |> expect_error("already in")
  tibble::tibble(x = 1:4, items = c(1:2, 1:2)) |> mapper(items = 1:2) |> expect_error("already in")
})

test_that("`manage_helper_col()` adds a column to a plain data frame with no rows", {
  mapper <- function(data, items = 1) manage_helper_col(data, items, 1)
  data.frame(x = numeric(0)) |> mapper(items = 2) |> colnames() |> expect_equal(c("x", "items"))
})


test_that("a helper column and an identical argument agree where both are missing", {
  # `near()` is `NA` for two missing values, which counted as a conflict:
  df <- tibble::tibble(x = c(5.19, 5.17), n = 28, items_averaged = c(1, NA))
  out <- df |> grim_map(digits_x = 2, items_averaged = c(1, NA), data_type = "integer")
  out$consistency |> expect_equal(c(FALSE, NA))
  df |> grim_map(digits_x = 2, items_averaged = c(NA, 1), data_type = "integer") |> expect_error("conflicts with")
  df |> grim_map(digits_x = 2, items_averaged = c(1, 2), data_type = "integer")  |> expect_error("conflicts with")
})


test_that("`manage_helper_col()` still takes `var_arg` and `affix`, with a warning", {
  rlang::local_options(lifecycle_verbosity = "warning")
  mapper_old <- function(data, items = 1) manage_helper_col(data, var_arg = items, default = 1, affix = FALSE)
  mapper_new <- function(data, items = 1) manage_helper_col(data, arg = items, default = 1, add_col = FALSE)
  df <- tibble::tibble(x = 1, items = 2)
  df |> mapper_old() |> expect_warning("var_arg") |> expect_warning("affix")
  df |> mapper_old() |> suppressWarnings() |> expect_equal(mapper_new(df))
  df |> mapper_old(items = 3) |> suppressWarnings() |> expect_error("items")
  df |> mapper_new(items = 3) |> expect_error("items")
  df |> manage_helper_col(items, 1, var_arg = items) |> expect_error("can't both be given")
})
