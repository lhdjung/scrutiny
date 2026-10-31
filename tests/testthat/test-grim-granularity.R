test_that("`grim_granularity()` returns a numeric value", {
  20 |> grim_granularity(1) |> expect_type("double")
  20 |> grim_granularity(2) |> expect_type("double")
  25 |> grim_granularity(1) |> expect_type("double")
  25 |> grim_granularity(2) |> expect_type("double")
})


test_that("`grim_items()` is deprecated", {
  rlang::local_options(lifecycle_verbosity = "warning")
  0.5 |> grim_items(2) |> expect_warning(class = "lifecycle_warning_deprecated")
})

test_that("A warning is thrown for item counts that are not whole numbers", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  20   |> grim_items(3)    |> expect_warning()
  47.3 |> grim_items(2)    |> expect_warning()
  47.3 |> grim_items(1:10) |> expect_warning()
})

test_that("The warning is not thrown for whole item counts", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  0.5 |> grim_items(2)   |> expect_silent()
  0.5 |> grim_items(0.5) |> expect_silent()
  0.1 |> grim_items(10)  |> expect_silent()
})


test_that("`grim_items()` passes missing counts through", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  c(NA, 20) |> grim_items(gran = 0.05) |> expect_no_warning()
  c(NA, 20) |> grim_items(gran = 0.05) |> expect_equal(c(NA, 1))
})


# `n = 0` used to return `Inf` without a word:
test_that("the granularity functions reject non-positive input", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  0  |> grim_items(gran = 0.05)  |> expect_error("`n` must be positive")
  20 |> grim_items(gran = -0.05) |> expect_error("`gran` must be positive")
  0  |> grim_granularity()       |> expect_error("`n` must be positive")
  20 |> grim_granularity(0)      |> expect_error("`items_averaged` must be positive")
})
