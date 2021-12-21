

x1 <- iris$Petal.Length
x2 <- mtcars$qsec
x3 <- rnorm(3000, 100, 15)

scalar_mapped_x1 <- x1 %>% purrr::map(decimal_places_scalar) %>% unlist()
scalar_mapped_x2 <- x2 %>% purrr::map(decimal_places_scalar) %>% unlist()
scalar_mapped_x3 <- x3 %>% purrr::map(decimal_places_scalar) %>% unlist()


test_that(
  "Both functions return the same (if `*_scalar` is mapped and unlisted)", {
  decimal_places(x1) %>% expect_equal(scalar_mapped_x1)
  decimal_places(x2) %>% expect_equal(scalar_mapped_x2)
  decimal_places(x3) %>% expect_equal(scalar_mapped_x3)
})

