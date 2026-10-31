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


test_that("a named matrix keeps a name whose header cell is `NA`", {
  m <- matrix(c(NA, "1", "b", "2"), nrow = 2, dimnames = list(NULL, c("keep", "q")))
  m |> row_to_colnames() |> colnames() |> expect_equal(c("keep", "b"))
})


test_that("`row` and `collapse` are checked", {
  df <- tibble::tibble(a = "x", b = "y")
  df |> row_to_colnames(row = Inf)      |> expect_error("whole numbers")
  df |> row_to_colnames(row = NA_real_) |> expect_error("whole numbers")
  df |> row_to_colnames(collapse = NULL) |> expect_error("character")
})

test_that("repeated header cells are an error, not duplicate names", {
  df <- tibble::tibble(V1 = c("Group", "A"), V2 = c("M", "5.3"), V3 = c("SD", "1.2"), V4 = c("M", "4.1"), V5 = c("SD", "0.9"))
  df |> row_to_colnames() |> expect_error("must be unique")
  df |> rbind(c(NA, "pre", "pre", "post", "post"), x = _) |> row_to_colnames(row = 1:2) |> colnames() |> expect_equal(c("Group", "pre M", "pre SD", "post M", "post SD"))
})
