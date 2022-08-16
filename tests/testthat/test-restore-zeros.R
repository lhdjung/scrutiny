

round_to <- rnorm(50, 2, 1) %>%
  censor(1, 4) %>%
  round()

numbers <- rnorm(50, 100, 30) %>%
  round(round_to) %>%
  restore_zeros(width = 4)


test_that("The number of decimal places checks out", {
  (decimal_places(numbers) == 4) %>% all() %>% expect_true()
})


test_that("The total number of characters checks out", {
  (stringr::str_length(numbers) - 5) %>% expect_equal(integer_places(numbers))
})
