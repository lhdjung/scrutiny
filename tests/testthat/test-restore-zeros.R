

round_to <- rnorm(50, 2, 1) %>%
  censor(0, 4) %>%
  round()

numbers <- rnorm(50, 100, 30) %>%
  round(round_to) %>%
  restore_zeros(width = 4)


test_that("The number of decimal places checks out", {
  expect_true(all(decimal_places(numbers) == 4))
})

test_that("The total number of characters checks out", {
  expect_true(all(stringr::str_length(numbers) - 5 == integer_places(numbers)))
})
