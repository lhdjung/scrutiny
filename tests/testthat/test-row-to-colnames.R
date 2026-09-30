a <- rnorm(30, 75, 10) |> round(1) |> as.character()
b <- rnorm(30, 75, 10) |> round(1) |> as.character()

df <- tibble::tibble(a, b) |>
  tibble::add_row(a = "a", b = "b", .before = 1)

colnames(df) <- c("Var1", "Var2")

df_fixed <- df |>
  row_to_colnames()


test_that("The correct column names are back", {
  df_fixed |> colnames() |> expect_equal(c("a", "b"))
})

test_that("The correct column names are no longer row values", {
  df_fixed |> purrr::pluck(1L, 1L) |> call_on(\(x) x == "a") |> expect_false()
  df_fixed |> purrr::pluck(2L, 1L) |> call_on(\(x) x == "b") |> expect_false()
})

test_that("missing header cells are skipped, not garbled", {
  tibble::tibble(
    V1 = c(NA, "a", "b"),
    V2 = c("age", "1", "2")
  ) |>
    row_to_colnames() |>
    colnames() |>
    expect_equal(c("V1", "age"))
  
  tibble::tibble(
    V1 = c("name", "first", "a"),
    V2 = c(NA, "age", "1"),
    V3 = c("x", "y", "z")
  ) |>
    row_to_colnames(row = 1:2) |>
    colnames() |>
    expect_equal(c("name first", "age", "x y"))
})


test_that("an unnamed matrix works quietly, and `row` must be in range", {
  m <- matrix(c("a", "1", "2", "b", "3", "4"), nrow = 3)
  m |> row_to_colnames() |> expect_silent()
  m |> row_to_colnames() |> expect_equal(tibble::tibble(a = c("1", "2"), b = c("3", "4")))
  m |> row_to_colnames(row = 0) |> expect_error("between 1 and")
  m |> row_to_colnames(row = 4) |> expect_error("between 1 and")
})
