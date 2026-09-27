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

df1_tested <- grim_map_total_n(df1, digits_x = 2)
df2_tested <- grim_map_total_n(df2, digits_x = 2)

df1_rec <- reverse_map_total_n(df1_tested)
df2_rec <- reverse_map_total_n(df2_tested)


test_that("The reconstructed data frames are identical to the original ones", {
  df1 |> expect_equal(df1_rec)
  df2 |> expect_equal(df2_rec)
})


test_that("The reconstruction does not depend on the first dispersion step", {
  # The total was taken as twice the second group size, right only for `0`:
  df <- tibble::tibble(x1 = c(3.43, 5.14), x2 = c(3.6, 2.95), n = c(43L, 40L))
  df |>
    grim_map_total_n(digits_x = 2, dispersion = 1:5) |>
    reverse_map_total_n() |>
    expect_equal(df)
})


test_that("`items` is not multiplied into the reconstructed total", {
  df <- tibble::tibble(x1 = 3.43, x2 = 4.20, n = 71L)
  out <- grim_map_total_n(df, digits_x = 2, items = 2)
  out$n[1:2] |> expect_equal(c(35L, 36L))
  out$items |> unique() |> expect_equal(2)
  reverse_map_total_n(out) |> expect_equal(df)
  audit_total_n(out)$n |> expect_equal(71L)
})


test_that("0-row output is reconstructed as 0 rows, keeping column types", {
  out <- grim_map_total_n(
    tibble::tibble(x1 = 3.4, x2 = 4.2, n = 3L),
    digits_x = 1,
    n_min = 2
  )
  out$x |> expect_type("double")
  rec <- reverse_map_total_n(out)
  nrow(rec) |> expect_equal(0L)
  rec$x1 |> expect_type("double")
  audit_total_n(out) |> nrow() |> expect_equal(0L)
})
