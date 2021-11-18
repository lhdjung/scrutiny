

x1 <- iris$Petal.Length
x2 <- mtcars$qsec
x3 <- rnorm(3000, 100, 15)

test_that("Both functions return the same", {
  expect_equal(
    decimal_places(x1),
    unlist(purrr::map(x1, decimal_places_scalar))
  )
  expect_equal(
    decimal_places(x2),
    unlist(purrr::map(x2, decimal_places_scalar))
  )
  expect_equal(
    decimal_places(x3),
    unlist(purrr::map(x3, decimal_places_scalar))
  )
})

