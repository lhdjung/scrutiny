

df1 <- pigs3
df1_tested <- debit_map(df1)


test_that("the output is a tibble", {
  df1_tested %>% tibble::is_tibble() %>% expect_true()
})

test_that("values are correctly tested for DEBIT-consistency", {
  df1_tested$consistency[df1_tested$consistency] %>% length() %>% expect_equal(6)
})

test_that("`show_rec` controls the number of columns", {
  pigs3 %>% debit_map(show_rec = TRUE ) %>% ncol() %>% expect_equal(11)
  pigs3 %>% debit_map(show_rec = FALSE) %>% ncol() %>% expect_equal(4)
})


test_that("an error is thrown if `extra` is misspecified", {
  pigs3 %>% debit_map(extra = blubb)   %>% expect_error()
  pigs3 %>% debit_map(extra = "blubb") %>% expect_error()
})


df1_expected <- tibble::tibble(
  x = c("0.53", "0.44", "0.77", "0.19", "0.34", "0.93", "0.12"),
  sd = c("0.50", "0.50", "0.42", "0.35", "0.47", "0.25", "0.33"),
  n = rep(1683L, 7L),
  consistency = rep(c(TRUE, FALSE, TRUE), c(3L, 1L, 3L)),
  rounding = rep("up_or_down", 7L),
  sd_lower = c(0.495, 0.495, 0.415, 0.345, 0.4649999999999999689138, 0.245, 0.325),
  sd_incl_lower = rep(TRUE, 7L),
  sd_upper = c(0.505, 0.505, 0.425, 0.355, 0.475, 0.255, 0.335),
  sd_incl_upper = rep(TRUE, 7L),
  x_lower = c(0.525, 0.435, 0.765, 0.185, 0.335, 0.925, 0.1149999999999999911182),
  x_upper = c(0.535, 0.445, 0.775, 0.195, 0.3450000000000000288658, 0.935, 0.125),
) %>%
  structure(
    class = c("scr_debit_map", "scr_rounding_up_or_down", "tbl_df", "tbl", "data.frame")
  )

test_that("`debit_map()` has correct output", {
  df1_tested %>% expect_equal(df1_expected)
})

