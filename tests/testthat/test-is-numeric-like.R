
test_that("`is_numeric_like()` handles base types correctly", {
  75.5 %>% is_numeric_like() %>% expect_true()
  999L %>% is_numeric_like() %>% expect_true()
  "42" %>% is_numeric_like() %>% expect_true()
  "ab" %>% is_numeric_like() %>% expect_false()
  TRUE %>% is_numeric_like() %>% expect_false()
})


test_that("`is_numeric_like()` handles factors correctly", {
  1:5     %>% factor() %>% is_numeric_like() %>% expect_true()
  letters %>% factor() %>% is_numeric_like() %>% expect_false()
  iris$Species         %>% is_numeric_like() %>% expect_false()
})


test_that("`is_numeric_like()` handles lists correctly", {
  list(1, 2, NA)        %>% is_numeric_like() %>% expect_true()
  list(1, 2, 3:7)       %>% is_numeric_like() %>% expect_false()
  list(1, 2, list(3:7)) %>% is_numeric_like() %>% expect_false()
})


test_that("`is_numeric_like()` handles non-vectors correctly", {
  # Testing a builtin, a closure, and an environment:
  length       %>% is_numeric_like() %>% expect_false()
  append       %>% is_numeric_like() %>% expect_false()
  rlang::env() %>% is_numeric_like() %>% expect_false()
})
