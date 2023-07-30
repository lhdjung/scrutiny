
# Expected output ---------------------------------------------------------

iris_exp <- tibble::tibble(
  term = c("count", "locations_n"),
  mean = c(9.74025974025974, 1.636363636363636464566),
  sd = c(9.917292973974667802395, 0.5106520833593400920947),
  median = c(7L, 2L),
  min = c(1L, 1L),
  max = c(50L, 3L),
)

nums <- c(1:10, 3:7, 9)

nums_exp <- tibble::tibble(
  term = "count",
  mean = 1.6,
  sd = 0.5163977794943221955037,
  median = 2,
  min = 1L,
  max = 2L,
)


# Testing -----------------------------------------------------------------

test_that("`audit()` for `duplicate_count()` works correcrtly by default", {
  iris %>% duplicate_count() %>% audit() %>% expect_equal(iris_exp)
  nums %>% duplicate_count() %>% audit() %>% expect_equal(nums_exp)
})
