test_that("`grim_granularity()` returns a numeric value", {
  20 |> grim_granularity(1) |> expect_type("double")
  20 |> grim_granularity(2) |> expect_type("double")
  25 |> grim_granularity(1) |> expect_type("double")
  25 |> grim_granularity(2) |> expect_type("double")
})


test_that("A warning is thrown for item counts that are not whole numbers", {
  20   |> grim_items(3)    |> expect_warning()
  47.3 |> grim_items(2)    |> expect_warning()
  47.3 |> grim_items(1:10) |> expect_warning()
})

test_that("The warning is not thrown for whole item counts", {
  0.5 |> grim_items(2)   |> expect_silent()
  0.5 |> grim_items(0.5) |> expect_silent()
  0.1 |> grim_items(10)  |> expect_silent()
})
