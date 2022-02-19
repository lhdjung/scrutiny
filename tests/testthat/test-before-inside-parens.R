

before <- rnorm(500, 25, 5) %>%
  round(2) %>%
  restore_zeros(width = 2)

inside <- rnorm(500, 25, 5) %>%
  round(2) %>%
  restore_zeros(width = 2)

x <- paste0(before, " (", inside, ")")


test_that("Substrings are extracted from the expected positions", {
  before_parens(x) %>% expect_equal(before)
  inside_parens(x) %>% expect_equal(inside)
})

test_that("Parentheses are removed", {
   before_parens(x) %>% stringr::str_detect("\\(") %>% any() %>% expect_false()
   inside_parens(x) %>% stringr::str_detect("\\(") %>% any() %>% expect_false()
   before_parens(x) %>% stringr::str_detect("\\)") %>% any() %>% expect_false()
   inside_parens(x) %>% stringr::str_detect("\\)") %>% any() %>% expect_false()
})
