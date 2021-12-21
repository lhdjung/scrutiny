

ranking <- seq_distance_df("5.37", n = 40) %>%
  grim_map() %>%
  seq_test_ranking() %>%
  suppressMessages()


test_that("The function returns a tibble", {
  ranking %>% tibble::is_tibble() %>% expect_true()
})

test_that("Dimensions are correct", {
  ranking$inconsistent[5:6] %>% is.na() %>% all() %>% expect_true()
  ranking$lead_lag[5:6]     %>% is.na() %>% all() %>% expect_true()
})

test_that("Values are correct", {
  ranking$consistent        %>% expect_equal(c(1, 2, 4, 6, 7, 9))
  ranking$inconsistent[1:4] %>% expect_equal(c(3, 5, 8, 10))
  ranking$lead_lag[1:4]     %>% expect_equal(c(2, 3, 4, 4))
})

