

test_that("`grim_total()` returns correct values", {
  grim_total("5.30", 20:30) %>% expect_equal(80:70)
})


test_that("`grim_total()` works with percentage conversion", {
  grim_total("87.50", 45:55, percent = TRUE) %>% expect_equal(9955:9945)
})



exp1 <- seq(from = 0.8, to = 0.7, by = -0.01)
exp2 <- seq(from = 0.9955, to = 0.9945, by = -0.0001)

test_that("`grim_ratio()` returns correct values", {
  grim_ratio("5.30", 20:30) %>% expect_equal(exp1)
})


test_that("`grim_ratio()` works with percentage conversion", {
  grim_ratio("87.50", 45:55, percent = TRUE) %>% expect_equal(exp2)
})


test_that("`grim_ratio_upper()` returns correct values", {
  grim_ratio_upper(0.1)   %>% expect_equal(0.9)
  grim_ratio_upper(0.11)  %>% expect_equal(0.99)
  grim_ratio_upper(0.111) %>% expect_equal(0.999)
})


test_that("`grim_ratio_upper()` works with percentage conversion", {
  grim_ratio_upper(0.1,   percent = TRUE) %>% expect_equal(0.999)
  grim_ratio_upper(0.11,  percent = TRUE) %>% expect_equal(0.9999)
  grim_ratio_upper(0.111, percent = TRUE) %>% expect_equal(0.99999)
})

