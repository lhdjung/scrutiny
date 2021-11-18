

test_that("`grim_granularity()` returns a numeric value", {
  expect_true(is.numeric(grim_granularity(20, 1)))
  expect_true(is.numeric(grim_granularity(20, 2)))
  expect_true(is.numeric(grim_granularity(25, 1)))
  expect_true(is.numeric(grim_granularity(25, 2)))
})


test_that("A warning is thrown for item counts that are not whole numbers", {
  expect_warning(grim_items(20, 3))
  expect_warning(grim_items(47.3, 2))
})

