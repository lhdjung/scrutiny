
# Long vectors with decimal numbers:
x1 <- iris$Petal.Length
x2 <- mtcars$qsec
x3 <- randu$y
x4 <- airquality$Wind
x5 <- attenu$accel

x6_digits <- rnorm(10000, 6, 3) %>%
  censor(0, 13) %>%
  round(0)

x6 <- rnorm(10000, 100, 15) %>%
  round(x6_digits)


out_expected_x1 <- c(
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
  1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
  1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
  1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0,
  1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1
)

out_expected_x2 <- c(
  2, 2, 2, 2, 2, 2, 2, 0, 1, 1, 1, 1, 1, 0, 2,
  2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 1, 1, 1, 1, 1,
  1, 1
)

out_expected_x3 <- c(
  6, 6, 6, 6, 6, 5, 5, 6, 6, 6, 6, 5, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5,
  6, 6, 6, 6, 6, 6, 6, 5, 6, 3, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 5, 6,
  6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 5,
  6, 6, 6, 6, 6, 6, 4, 6, 6, 6, 6, 6, 6, 5, 6,
  5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 5, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 5, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 5, 6, 6, 6,
  6, 4, 5, 6, 6, 6, 6, 6, 6, 4, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5,
  6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 5,
  6, 6, 6, 5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 5, 6,
  6, 6, 6, 6, 6, 6, 5, 5, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5,
  6, 6, 6, 6, 6, 5, 6, 6, 5, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6
)

out_expected_x4 <- c(
  1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1,
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
  1, 0, 1
)

out_expected_x5 <- c(
  3, 3, 3, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 2, 3, 3, 3, 2,
  3, 2, 2, 2, 2, 2, 3, 2, 3, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 3, 2, 1, 2, 2, 3, 3, 3, 3, 2, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 3, 2, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 2, 3, 2, 2, 3, 2, 2, 3, 2, 2, 3,
  3, 3
)


test_that("`decimal_places()` counts accurately", {
  x1 %>% decimal_places() %>% expect_equal(out_expected_x1)
  x2 %>% decimal_places() %>% expect_equal(out_expected_x2)
  x3 %>% decimal_places() %>% expect_equal(out_expected_x3)
  x4 %>% decimal_places() %>% expect_equal(out_expected_x4)
  x5 %>% decimal_places() %>% expect_equal(out_expected_x5)
})



out_scalar_x1 <- x1 %>% purrr::map_int(decimal_places_scalar)
out_scalar_x2 <- x2 %>% purrr::map_int(decimal_places_scalar)
out_scalar_x3 <- x3 %>% purrr::map_int(decimal_places_scalar)
out_scalar_x4 <- x4 %>% purrr::map_int(decimal_places_scalar)
out_scalar_x5 <- x5 %>% purrr::map_int(decimal_places_scalar)
out_scalar_x6 <- x6 %>% purrr::map_int(decimal_places_scalar)


test_that("Both functions return the same count for each individual number", {
  x1 %>% decimal_places() %>% expect_equal(out_scalar_x1)
  x2 %>% decimal_places() %>% expect_equal(out_scalar_x2)
  x3 %>% decimal_places() %>% expect_equal(out_scalar_x3)
  x4 %>% decimal_places() %>% expect_equal(out_scalar_x4)
  x5 %>% decimal_places() %>% expect_equal(out_scalar_x5)
  x6 %>% decimal_places() %>% expect_equal(out_scalar_x6)
})




test_that("`decimal_places_scalar()` consitions work as expected", {
  25 %>% decimal_places_scalar() %>% expect_equal(0)
  NA %>% decimal_places_scalar() %>% expect_equal(NA)
})




