

test_that("`grim_granularity()` returns a numeric value", {
  grim_granularity(20, 1) %>% is.numeric() %>% expect_true()
  grim_granularity(20, 2) %>% is.numeric() %>% expect_true()
  grim_granularity(25, 1) %>% is.numeric() %>% expect_true()
  grim_granularity(25, 2) %>% is.numeric() %>% expect_true()
})


test_that("A warning is thrown for item counts that are not whole numbers", {
  grim_items(20, 3)      %>% expect_warning()
  grim_items(47.3, 2)    %>% expect_warning()
  grim_items(47.3, 1:10) %>% expect_warning()
})

test_that("The warning is not thrown for whole item counts", {
  grim_items(0.5, 2)   %>% expect_silent()
  grim_items(0.5, 0.5) %>% expect_silent()
  grim_items(0.1, 10)  %>% expect_silent()
})

