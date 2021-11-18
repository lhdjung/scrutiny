

before <- rnorm(500, 25, 5) %>%
  round(2) %>%
  restore_zeros(width = 2)

inside <- rnorm(500, 25, 5) %>%
  round(2) %>%
  restore_zeros(width = 2)

x <- paste0(before, " (", inside, ")")



test_that("Substrings are extracted from the expected positions", {
  expect_equal(before_parens(x), before)
  expect_equal(inside_parens(x), inside)
})


test_that("Parentheses are removed", {
   expect_false(any(stringr::str_detect(before_parens(x), "\\(")))
   expect_false(any(stringr::str_detect(inside_parens(x), "\\(")))
   expect_false(any(stringr::str_detect(before_parens(x), "\\)")))
   expect_false(any(stringr::str_detect(inside_parens(x), "\\)")))
})

