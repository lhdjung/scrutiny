

ranking <- seq_distance_df("5.37", n = 40) %>%
  grim_map() %>%
  seq_test_ranking() %>%
  suppressMessages()


test_that("Dimensions are correct", {
  expect_true(is.data.frame(ranking))
  expect_true(all(is.na(ranking$inconsistent[5:6])))
  expect_true(all(is.na(ranking$lead_lag[5:6])))
})

test_that("Values are correct", {
  expect_equal(ranking$consistent, c(1, 2, 4, 6, 7, 9))
  expect_equal(ranking$inconsistent[1:4], c(3, 5, 8, 10))
  expect_equal(ranking$lead_lag[1:4], c(2, 3, 4, 4))
})

