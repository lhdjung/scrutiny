
a <- rnorm(30, 75, 10) %>% round(1) %>% as.character()
b <- rnorm(30, 75, 10) %>% round(1) %>% as.character()

df <- tibble::tibble(a, b) %>%
  tibble::add_row(a = "a", b = "b", .before = 1)

colnames(df) <- c("Var1", "Var2")

df_fixed <- df %>%
  row_to_colnames()


test_that("The correct column names are back", {
  df_fixed %>% colnames() %>% expect_equal(c("a", "b"))
})

test_that("The correct column names are no longer row values", {
  df_fixed[1, ][[1]] %>% `==`("a") %>% expect_false()
  df_fixed[1, ][[2]] %>% `==`("b") %>% expect_false()
})
