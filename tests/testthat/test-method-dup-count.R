
# Expected output ---------------------------------------------------------

iris_exp <- tibble::tibble(
  term = c("count", "locations_n"),
  mean = c(9.74025974025974, 1.636363636363636464566),
  sd = c(9.917292973974667802395, 0.5106520833593400920947),
  median = c(7, 2),
  min = c(1, 1),
  max = c(50, 3),
  na_count = c(0, 0),
  na_rate = c(0, 0),
)

mtcars_exp <- tibble::tibble(
  term = c("count", "locations_n"),
  mean = c(2.227848101265822666761, 1.082278481012658222227),
  sd = c(4.980086119759305596233, 0.3379441380253598303796),
  median = c(1, 1),
  min = c(1, 1),
  max = c(37, 3),
  na_count = c(0, 0),
  na_rate = c(0, 0),
)

nums <- c(1:10, 3:7, 9)

nums_exp <- tibble::tibble(
  term = "count",
  mean = 1.6,
  sd = 0.5163977794943221955037,
  median = 2,
  min = 1,
  max = 2,
  na_count = 0,
  na_rate = 0,
)


# Testing -----------------------------------------------------------------

test_that("`audit()` for `duplicate_count()` works correctly by default", {
  iris   %>% duplicate_count() %>% audit() %>% expect_equal(iris_exp)
  mtcars %>% duplicate_count() %>% audit() %>% expect_equal(mtcars_exp)
  nums   %>% duplicate_count() %>% audit() %>% expect_equal(nums_exp)
})
