
# Expected output ---------------------------------------------------------

pigs4_exp <- tibble::tibble(
  term = c("count", "total_x", "total_y", "rate_x", "rate_y"),
  mean = c(1.333333333333333259318, 5, 5, 0.2666666666666666629659, 0.2666666666666666629659),
  sd = c(0.5773502691896257310589, 0, 0, 0.1154700538379251628651, 0.1154700538379251628651),
  median = c(1, 5, 5, 0.2, 0.2),
  min = c(1, 5, 5, 0.2, 0.2),
  max = c(2, 5, 5, 0.4, 0.4),
  na_count = numeric(5),
  na_rate = numeric(5),
)

iris_exp <- tibble::tibble(
  term = c("count", "total_x", "total_y", "rate_x", "rate_y"),
  mean = c(25.4, 150, 150, 0.1693333333333333357018, 0.1693333333333333357018),
  sd = c(41.22081459112077794771, 0, 0, 0.2748054306074718677877, 0.2748054306074718677877),
  median = c(1.5, 150, 150, 0.01, 0.01),
  min = c(0, 150, 150, 0, 0),
  max = c(125, 150, 150, 0.8333333333333333703408, 0.8333333333333333703408),
  na_count = numeric(5),
  na_rate = numeric(5),
)

mtcars_exp <- tibble::tibble(
  term = c("count", "total_x", "total_y", "rate_x", "rate_y"),
  mean = c(
    2.472727272727272662678, 32, 32, 0.07727272727272727070869,
    0.07727272727272727070869
  ),
  sd = c(7.38079225260366467154, 0, 0, 0.2306497578938645209856, 0.2306497578938645209856),
  median = c(0, 32, 32, 0, 0),
  min = c(0, 32, 32, 0, 0),
  max = rep(c(32, 1), 3:2),
  na_count = numeric(5),
  na_rate = numeric(5),
)


# Testing -----------------------------------------------------------------

test_that("`audit()` for `duplicate_count_colpair()` works correctly", {
  pigs4  %>% duplicate_count_colpair() %>% audit() %>% expect_equal(pigs4_exp)
  iris   %>% duplicate_count_colpair() %>% audit() %>% expect_equal(iris_exp)
  mtcars %>% duplicate_count_colpair() %>% audit() %>% expect_equal(mtcars_exp)
})
