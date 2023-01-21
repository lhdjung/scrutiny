

x_orig <- seq(from = 0.1, to = 0.5, by = 0.1)
x <- x_orig


test_that("numeric values are handled correctly by the prefix form", {
  x %>% seq_length(10) %>% expect_equal(seq(from = 0.1, to = 1, by = 0.1))
  x %>% seq_length(0)  %>% expect_equal(numeric(0))
})


test_that("strings are handled correctly by the prefix form", {
  x %>% as.character() %>% seq_length(10) %>% expect_equal(seq_endpoint(from = 0.1, to = 1))
  x %>% as.character() %>% seq_length(0)  %>% expect_equal(character(0))
})


test_that("numeric values are handled correctly by the replacement form", {
  seq_length(x) <- 10
  x %>% expect_equal(seq(from = 0.1, to = 1, by = 0.1))
  seq_length(x) <- 0
  x %>% expect_equal(numeric(0))
})


test_that("strings are handled correctly by the replacement form", {
  x <- as.character(x_orig)
  seq_length(x) <- 10
  x %>% expect_equal(seq_endpoint(from = 0.1, to = 1))
  seq_length(x) <- 0
  x %>% expect_equal(character(0))
})
