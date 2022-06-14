

df1 <- pigs3
df1_tested <- debit_map(df1)


test_that("The output is a tibble", {
  df1_tested %>% tibble::is_tibble() %>% expect_true()
})

test_that("Values are correctly tested for DEBIT-consistency", {
  df1_tested$consistency[df1_tested$consistency] %>% length() %>% expect_equal(6)
})


test_that("Types are what they should be", {
df1_tested$x             %>% expect_type("character")
df1_tested$sd            %>% expect_type("character")
df1_tested$n             %>% expect_type("integer")
df1_tested$consistency   %>% expect_type("logical")
df1_tested$rounding      %>% expect_type("character")
df1_tested$sd_lower      %>% expect_type("double")
df1_tested$sd_incl_lower %>% expect_type("logical")
df1_tested$sd_upper      %>% expect_type("double")
df1_tested$sd_incl_upper %>% expect_type("logical")
df1_tested$x_lower       %>% expect_type("double")
df1_tested$x_upper       %>% expect_type("double")
})


test_that("`show_rec` controls the number of columns", {
  pigs3 %>% debit_map(show_rec = TRUE ) %>% ncol() %>% expect_equal(11)
  pigs3 %>% debit_map(show_rec = FALSE) %>% ncol() %>% expect_equal(4)
})


test_that("An error is thrown if `extra` is misspecified", {
  pigs3 %>% debit_map(extra = blubb)   %>% expect_error()
  pigs3 %>% debit_map(extra = "blubb") %>% expect_error()
})



# df2 <- df1 %>%
#   dplyr::rename(
#     mean = x,
#     std_dev = sd,
#     sample_size = n
#   )
#
# df2_tested <- df2 %>%
#   debit_map(x = mean, sd = std_dev, n = sample_size)
#
#
# test_that("The `x`, `sd`, and `n` arguments associate with
#           their respective columns correctly", {
#   df2_tested %>% expect_equal(df1_tested)
# })
#
#
# test_that("Wrong `extra` specifications lead to an error", {
#   df1 %>% debit_map(extra = 5) %>% expect_error()
# })
#
#
#
# df3 <- df1 %>%
#   dplyr::mutate(bla = 4)
#
# df3_tested <- df3 %>%
#   debit_map()
#
# df1_tested_bla <- df1_tested %>%
#   dplyr::mutate(bla = 4)
#
#
# test_that("", {
#   df3_tested %>% expect_equal(df1_tested_bla)
# })



