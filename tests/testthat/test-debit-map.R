
data <- debit_map(pigs3)

test_that("`debit_map()` tests data frames for DEBIT consistency", {
  expect_true(is.data.frame(data))
  expect_equal(length(data$consistency[data$consistency]), 6)
})



test_that("Types are what they should be", {
data$x             %>% is.character() %>% expect_true()
data$sd            %>% is.character() %>% expect_true()
data$n             %>% is.integer() %>% expect_true()
data$consistency   %>% is.logical() %>% expect_true()
data$rounding      %>% is.character() %>% expect_true()
data$sd_lower      %>% is.double() %>% expect_true()
data$sd_incl_lower %>% is.logical() %>% expect_true()
data$sd_upper      %>% is.double() %>% expect_true()
data$sd_incl_upper %>% is.logical() %>% expect_true()
data$x_lower       %>% is.double() %>% expect_true()
data$x_upper       %>% is.double() %>% expect_true()
})



test_that("`show_intermed` controls the number of columns", {
  expect_equal(ncol(debit_map(pigs3, show_intermed = TRUE)), 11)
  expect_equal(ncol(debit_map(pigs3, show_intermed = FALSE)), 4)
})


