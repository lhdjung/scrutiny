

# Basic linear testing function -------------------------------------------

test_that("`is_seq_linear()` returns `TRUE` when it should", {
  1             %>% is_seq_linear() %>% expect_true()
  1:10          %>% is_seq_linear() %>% expect_true()
  c(3, 10)      %>% is_seq_linear() %>% expect_true()
  seq(4, 12, 2) %>% is_seq_linear() %>% expect_true()
})


test_that("`is_seq_linear()` returns `FALSE` when it should", {
  c(1:10, 12)  %>% is_seq_linear() %>% expect_false()
  c(3, 12, 14) %>% is_seq_linear() %>% expect_false()
  c(7, 2, 20)  %>% is_seq_linear() %>% expect_false()
})




# With `test_linear = TRUE` (the default) ---------------------------------

test_that("`is_seq_ascending()` with the default `test_linear = TRUE`
          returns `TRUE` when it should", {
  1:10          %>% is_seq_ascending() %>% expect_true()
  c(3, 10)      %>% is_seq_ascending() %>% expect_true()
  seq(4, 12, 2) %>% is_seq_ascending() %>% expect_true()
})


test_that("`is_seq_ascending()` with the default `test_linear = TRUE`
          returns `FALSE` when it should", {
  1            %>% is_seq_ascending() %>% expect_false()
  c(1:10, 12)  %>% is_seq_ascending() %>% expect_false()
  c(3, 12, 14) %>% is_seq_ascending() %>% expect_false()
  c(7, 2, 20)  %>% is_seq_ascending() %>% expect_false()
})


test_that("`is_seq_descending()` with the default `test_linear = TRUE`
          returns `TRUE` when it should", {
  1:10          %>% rev() %>% is_seq_descending() %>% expect_true()
  c(3, 10)      %>% rev() %>% is_seq_descending() %>% expect_true()
  seq(4, 12, 2) %>% rev() %>% is_seq_descending() %>% expect_true()
})


test_that("`is_seq_descending()` with the default `test_linear = TRUE`
          returns `FALSE` when it should", {
  1                      %>% is_seq_descending() %>% expect_false()
  c(1:10, 12)  %>% rev() %>% is_seq_descending() %>% expect_false()
  c(3, 12, 14) %>% rev() %>% is_seq_descending() %>% expect_false()
  c(7, 2, 20)  %>% rev() %>% is_seq_descending() %>% expect_false()
})


test_that("`is_seq_dispersed()` with the default `test_linear = TRUE`
          returns `FALSE` when it should", {
  seq_disperse(50)    %>% is_seq_dispersed(from = 50)    %>% expect_true()
  seq_disperse(7.3)   %>% is_seq_dispersed(from = 7.3)   %>% expect_true()
  seq_disperse(0.009) %>% is_seq_dispersed(from = 0.009) %>% expect_true()
})


test_that("`is_seq_dispersed()` with the default `test_linear = TRUE`
          returns `FALSE` when it should", {
  1             %>% is_seq_dispersed(from = 3) %>% expect_false()
  1:10          %>% is_seq_dispersed(from = 3) %>% expect_false()
  c(3, 10)      %>% is_seq_dispersed(from = 3) %>% expect_false()
  seq(4, 12, 2) %>% is_seq_dispersed(from = 3) %>% expect_false()
})




# With `test_linear = FALSE` ----------------------------------------------

f <- FALSE

test_that("`is_seq_ascending()` with `test_linear = FALSE`
          returns `TRUE` when it should", {
  1:10          %>% is_seq_ascending(test_linear = f) %>% expect_true()
  c(3, 10)      %>% is_seq_ascending(test_linear = f) %>% expect_true()
  c(1:10, 12)   %>% is_seq_ascending(test_linear = f) %>% expect_true()
  c(3, 12, 14)  %>% is_seq_ascending(test_linear = f) %>% expect_true()
  seq(4, 12, 2) %>% is_seq_ascending(test_linear = f) %>% expect_true()
})


test_that("`is_seq_ascending()` with `test_linear = FALSE`
          returns `FALSE` when it should", {
  1            %>% is_seq_ascending(test_linear = f) %>% expect_false()
  c(7, 2, 20)  %>% is_seq_ascending(test_linear = f) %>% expect_false()
})


test_that("`is_seq_descending()` with `test_linear = FALSE`
          returns `TRUE` when it should", {
  1:10          %>% rev() %>% is_seq_descending(test_linear = f) %>% expect_true()
  c(3, 10)      %>% rev() %>% is_seq_descending(test_linear = f) %>% expect_true()
  seq(4, 12, 2) %>% rev() %>% is_seq_descending(test_linear = f) %>% expect_true()
  c(1:10, 12)   %>% rev() %>% is_seq_descending(test_linear = f) %>% expect_true()
  c(3, 12, 14)  %>% rev() %>% is_seq_descending(test_linear = f) %>% expect_true()
})


test_that("`is_seq_descending()` with `test_linear = FALSE`
          returns `FALSE` when it should", {
  1            %>% rev() %>% is_seq_descending(test_linear = f) %>% expect_false()
  c(7, 2, 20)  %>% rev() %>% is_seq_descending(test_linear = f) %>% expect_false()
})


test_that("`is_seq_dispersed()` with `test_linear = FALSE`
          returns `FALSE` when it should", {
  seq_disperse(50)    %>% is_seq_dispersed(from = 50   , test_linear = f) %>% expect_true()
  seq_disperse(7.3)   %>% is_seq_dispersed(from = 7.3  , test_linear = f) %>% expect_true()
  seq_disperse(0.009) %>% is_seq_dispersed(from = 0.009, test_linear = f) %>% expect_true()
})


test_that("`is_seq_dispersed()` with `test_linear = FALSE`
          returns `FALSE` when it should", {
  1             %>% is_seq_dispersed(from = 3, test_linear = f) %>% expect_false()
  1:10          %>% is_seq_dispersed(from = 3, test_linear = f) %>% expect_false()
  c(3, 10)      %>% is_seq_dispersed(from = 3, test_linear = f) %>% expect_false()
  seq(4, 12, 2) %>% is_seq_dispersed(from = 3, test_linear = f) %>% expect_false()
})



# Special tests for `is_seq_dispersed()` ----------------------------------

test_that("`is_seq_dispersed()` passes its special tests, returning `TRUE`", {
  c(3:7) %>% is_seq_dispersed(from = 5, test_linear = f) %>% expect_true()
})


test_that("`is_seq_dispersed()` passes its special tests, returning `NA`", {
  c(NA, 3:7, NA)         %>% is_seq_dispersed(from = 5, test_linear = f) %>% is.na() %>% expect_true()
  c(NA, NA, 3:7, NA, NA) %>% is_seq_dispersed(from = 5, test_linear = f) %>% is.na() %>% expect_true()
  c(NA, 3:6, NA, NA)     %>% is_seq_dispersed(from = 5, test_linear = f) %>% is.na() %>% expect_true()
})


test_that("`is_seq_dispersed()` passes its special tests, returning `FALSE`", {
  c(NA, 3:7)     %>% is_seq_dispersed(from = 5, test_linear = f) %>% expect_false()
  c(3:7, NA)     %>% is_seq_dispersed(from = 5, test_linear = f) %>% expect_false()
  c(3:7, NA, NA) %>% is_seq_dispersed(from = 5, test_linear = f) %>% expect_false()
  c(NA, NA, 3:7) %>% is_seq_dispersed(from = 5, test_linear = f) %>% expect_false()
})

