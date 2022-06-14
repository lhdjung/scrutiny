

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

