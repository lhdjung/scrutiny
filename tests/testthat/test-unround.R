
df <- unround(c(3.6, "5.20", 5.174)) %>%
  suppressMessages()


test_that("The output is a tibble", {
  tibble::is_tibble(df) %>% expect_true()
})

test_that("It has correct dimensions", {
  (nrow(df) == 3) %>% expect_true()
  (ncol(df) == 7) %>% expect_true()
})


colnames_expected <- c(
  "range", "rounding", "lower", "incl_lower", "x", "incl_upper", "upper"
)

test_that("It has correct column names", {
  (colnames(df) == colnames_expected) %>% all() %>% expect_true()
})

test_that("Its columns have the correct types", {
  df[[1]] %>% is.character() %>% expect_true()
  df[[2]] %>% is.character() %>% expect_true()
  df[[3]] %>% is.numeric()   %>% expect_true()
  df[[4]] %>% is.logical()   %>% expect_true()
  df[[5]] %>% is.character() %>% expect_true()
  df[[6]] %>% is.logical()   %>% expect_true()
  df[[7]] %>% is.numeric()   %>% expect_true()
})
