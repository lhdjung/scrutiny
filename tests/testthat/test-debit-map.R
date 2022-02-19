
df_tested <- debit_map(pigs3)


test_that("The output is a tibble", {
  df_tested %>% tibble::is_tibble() %>% expect_true()
})

test_that("Values are correctly tested for DEBIT-consistency", {
  df_tested$consistency[df_tested$consistency] %>% length() %>% expect_equal(6)
})


test_that("Types are what they should be", {
df_tested$x             %>% is.character() %>% expect_true()
df_tested$sd            %>% is.character() %>% expect_true()
df_tested$n             %>% is.integer() %>% expect_true()
df_tested$consistency   %>% is.logical() %>% expect_true()
df_tested$rounding      %>% is.character() %>% expect_true()
df_tested$sd_lower      %>% is.double() %>% expect_true()
df_tested$sd_incl_lower %>% is.logical() %>% expect_true()
df_tested$sd_upper      %>% is.double() %>% expect_true()
df_tested$sd_incl_upper %>% is.logical() %>% expect_true()
df_tested$x_lower       %>% is.double() %>% expect_true()
df_tested$x_upper       %>% is.double() %>% expect_true()
})


test_that("`show_rec` controls the number of columns", {
  pigs3 %>% debit_map(show_rec = TRUE ) %>% ncol() %>% expect_equal(11)
  pigs3 %>% debit_map(show_rec = FALSE) %>% ncol() %>% expect_equal(4)
})


