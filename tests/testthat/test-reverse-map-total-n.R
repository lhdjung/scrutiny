# fmt: skip
df1 <- tibble::tribble(
  ~x1  , ~x2  , ~n  ,
  3.43 , 5.28 ,  90 ,
  2.97 , 4.42 , 103
)

df2 <- tibble::tibble(
  x1 = runif(150, 1, 100) |> round(2),
  x2 = runif(150, 50, 150) |> round(2),
  n = runif(150, 20, 120) |> round()
)

df1_tested <- grim_map_total_n(
  df1,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)
df2_tested <- grim_map_total_n(
  df2,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)

df1_rec <- reverse_map_total_n(df1_tested)
df2_rec <- reverse_map_total_n(df2_tested)


test_that("The reconstructed data frames are identical to the original ones", {
  df1 |> expect_equal(df1_rec)
  df2 |> expect_equal(df2_rec)
})


test_that("a case whose first pair is a back pair keeps its groups in order", {
  # The back direction swaps the groups, so the first row of a back pair holds
  # `x2`; these used to be returned as `x1 = 5.28, x2 = 3.43`:
  df1_tested |> dplyr::filter(dir == "back")                 |> reverse_map_total_n() |> expect_equal(df1)
  df1_tested |> dplyr::arrange(case, dplyr::desc(dir))       |> reverse_map_total_n() |> expect_equal(df1)
  df1_tested |> dplyr::filter(dir == "back")                 |> audit() |> dplyr::select(x1, x2, n) |> unclass_scr() |> expect_equal(df1)
  # GRIMMER, with a second pair of reported columns:
  df_sd <- tibble::tibble(x1 = 3.43, x2 = 5.28, sd1 = 1.21, sd2 = 0.95, n = 90)
  df_sd |>
    grimmer_map_total_n(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    dplyr::filter(dir == "back") |>
    reverse_map_total_n() |>
    expect_equal(df_sd)
})


test_that("The reconstruction does not depend on the first dispersion step", {
  # The total was taken as twice the second group size, right only for `0`:
  df <- tibble::tibble(x1 = c(3.43, 5.14), x2 = c(3.6, 2.95), n = c(43L, 40L))
  df |>
    grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 1:5) |>
    reverse_map_total_n() |>
    expect_equal(df)
})


test_that("`items_averaged` is not multiplied into the reconstructed total", {
  df <- tibble::tibble(x1 = 3.43, x2 = 4.20, n = 71L)
  out <- grim_map_total_n(df, digits_x = 2, items_averaged = 2, data_type = "integer")
  out$n[1:2] |> expect_equal(c(35L, 36L))
  out$items_averaged |> unique() |> expect_equal(2)
  out |> reverse_map_total_n() |> expect_equal(df)
  out |> audit() |> purrr::pluck("n") |> expect_equal(71L)
})


test_that("0-row output is reconstructed as 0 rows, keeping column types", {
  out <- grim_map_total_n(
    tibble::tibble(x1 = 3.4, x2 = 4.2, n = 3L),
    digits_x = 1,
    items_averaged = 1,
    data_type = "integer",
    n_min = 2
  ) |>
    suppressWarnings()
  out$x |> expect_type("double")
  rec <- reverse_map_total_n(out)
  rec |> nrow() |> expect_equal(0L)
  rec$x1 |> expect_type("double")
  out |> audit() |> nrow() |> expect_equal(0L)
})


test_that("incomplete mapper output is rejected", {
  # The first two rows of each case used to be paired up whatever they were:
  out <- tibble::tibble(x1 = 4.52, x2 = 5.23, n = 40) |>
    grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer")
  out[-1, ]                           |> reverse_map_total_n() |> expect_error("in full")
  out[-c(2, 3), ]                     |> reverse_map_total_n() |> expect_error("in full")
  out |> dplyr::filter(consistency)   |> reverse_map_total_n() |> expect_error("in full")
  out |> dplyr::arrange(n)            |> reverse_map_total_n() |> expect_error("in order")
})


# The reported columns used to be the ones before `n`, so a custom basic mapper
# with `n` first rebuilt `n1` and `n2`, and one with `n` in the middle lost the
# columns after it:
test_that("the reported columns are found wherever `n` is", {
  is_even_sum <- function(n, y) (round(y * n) %% 2) == 0
  map_n_first <- function_map(is_even_sum, .reported = c("n", "y"), .name_test = "PAR")
  map_n_first_total_n <- function_map_total_n(map_n_first, .reported = "y", .name_test = "PAR")
  data <- tibble::tibble(y1 = 1.5, y2 = 2.25, n = 12L)
  data |> map_n_first_total_n(dispersion = 0:1) |> reverse_map_total_n() |> expect_equal(data)

  always <- function(x, n, sd) TRUE
  map_n_middle <- function_map(always, .reported = c("x", "n", "sd"), .name_test = "ALW")
  map_n_middle_total_n <- function_map_total_n(map_n_middle, .reported = c("x", "sd"), .name_test = "ALW")
  data <- tibble::tibble(x1 = 1, x2 = 2, sd1 = 3, sd2 = 4, n = 10L)
  data |> map_n_middle_total_n() |> reverse_map_total_n() |> expect_equal(data)
})

test_that("a missing column or a lost attribute is a scrutiny error", {
  out <- tibble::tibble(x1 = c(5.19, 4.56), x2 = c(4.5, 1.2), n = c(28, 31)) |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 0:1)
  out |> dplyr::select(-both_consistent) |> audit()               |> expect_error("Missing: `both_consistent`")
  out |> dplyr::select(-dir)             |> audit()               |> expect_error("Missing: `dir`")
  out |> dplyr::select(-n_change)        |> audit()               |> expect_error("Missing: `n_change`")
  out |> dplyr::select(-case)            |> audit()               |> expect_error("Missing: `case`")
  out |> dplyr::select(-x)               |> reverse_map_total_n() |> expect_error("Missing: `x`")
  attr(out, "scrutiny") <- NULL
  out |> audit()               |> expect_error("lost the settings")
  out |> reverse_map_total_n() |> expect_error("lost the settings")
})
