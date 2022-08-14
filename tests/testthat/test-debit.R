
# x <- rnorm(1000, 0.5, 0.08) %>% censor(0, 1) %>% as.character()
# sd <- runif(1000, 0.1, 0.4) %>% as.character()
# n <- rnorm(1000, 1000, 200) %>% censor(100, 1900)
#
#
# x <- seq_endpoint("0.01", 1) %>% rep(10)
# sd <- rnorm(100, 0.15, 0.05) %>% round(2) %>% restore_zeros(width = 2)
#
# out <- purrr::pmap_lgl(list(x, sd, n), debit)



out <- purrr::pmap_lgl(pigs3, debit)
out_expected <- c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)

test_that("bla", {
  out %>% expect_equal(out_expected)
})
