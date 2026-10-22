test_that("`check_audit_special()` points to the special summaries", {
  df_total_n <- tibble::tibble(x1 = 4.74, x2 = 2.57, n = 50L)

  pigs1      |> grim_map(digits_x = 2, items_averaged = 1)         |> check_audit_special("GRIM") |> expect_no_message()
  pigs1      |> grim_map_seq(digits_x = 2, items_averaged = 1)     |> check_audit_special("GRIM") |> expect_message("audit_seq")
  df_total_n |> grim_map_total_n(digits_x = 2, items_averaged = 1) |> check_audit_special("GRIM") |> expect_message("audit_total_n")
})

test_that("`manage_helper_col()` takes the helper from the column or the argument", {
  mapper <- function(data, items = 1) manage_helper_col(data, items, 1)
  mapper_no_affix <- function(data, items = 1) manage_helper_col(data, items, 1, affix = FALSE)

  tibble::tibble(x = 5.19)            |> mapper(items = 2)          |> expect_equal(tibble::tibble(x = 5.19, items = 2))
  tibble::tibble(x = 5.19)            |> mapper()                   |> expect_equal(tibble::tibble(x = 5.19, items = 1))
  tibble::tibble(x = 5.19)            |> mapper_no_affix(items = 2) |> expect_equal(tibble::tibble(x = 5.19))
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
