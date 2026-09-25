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
