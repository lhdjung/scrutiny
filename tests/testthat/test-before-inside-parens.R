

before <- rnorm(500, 25, 5) %>%
  round(2) %>%
  restore_zeros(width = 2)

inside <- rnorm(500, 25, 5) %>%
  round(2) %>%
  restore_zeros(width = 2)

x_parens <- paste0(before, " (", inside, ")")

x_brackets <- x_parens %>%
  stringr::str_replace("\\(", "[") %>%
  stringr::str_replace("\\)", "]")

x_braces <- x_parens %>%
  stringr::str_replace("\\(", "{") %>%
  stringr::str_replace("\\)", "}")


test_that("With parentheses, substrings are extracted
          from the expected positions", {
  x_parens %>% before_parens(sep = "parens") %>% expect_equal(before)
  x_parens %>% inside_parens(sep = "parens") %>% expect_equal(inside)
})

test_that("With parentheses, the separators are removed", {
  x_parens %>% before_parens(sep = "parens") %>% stringr::str_detect("\\(") %>% any() %>% expect_false()
  x_parens %>% inside_parens(sep = "parens") %>% stringr::str_detect("\\(") %>% any() %>% expect_false()
  x_parens %>% before_parens(sep = "parens") %>% stringr::str_detect("\\)") %>% any() %>% expect_false()
  x_parens %>% inside_parens(sep = "parens") %>% stringr::str_detect("\\)") %>% any() %>% expect_false()
})


test_that("With square brackets, substrings are extracted
          from the expected positions", {
  x_brackets %>% before_parens(sep = "brackets") %>% expect_equal(before)
  x_brackets %>% inside_parens(sep = "brackets") %>% expect_equal(inside)
})

test_that("With square brackets, the separators are removed", {
  x_brackets %>% before_parens(sep = "brackets") %>% stringr::str_detect("\\[") %>% any() %>% expect_false()
  x_brackets %>% inside_parens(sep = "brackets") %>% stringr::str_detect("\\[") %>% any() %>% expect_false()
  x_brackets %>% before_parens(sep = "brackets") %>% stringr::str_detect("\\]") %>% any() %>% expect_false()
  x_brackets %>% inside_parens(sep = "brackets") %>% stringr::str_detect("\\]") %>% any() %>% expect_false()
})



test_that("With curly braces, substrings are extracted
          from the expected positions", {
   x_braces %>% before_parens(sep = "braces") %>% expect_equal(before)
   x_braces %>% inside_parens(sep = "braces") %>% expect_equal(inside)
})

test_that("With curly braces, the separators are removed", {
  x_braces %>% before_parens(sep = "braces") %>% stringr::str_detect("\\{") %>% any() %>% expect_false()
  x_braces %>% inside_parens(sep = "braces") %>% stringr::str_detect("\\{") %>% any() %>% expect_false()
  x_braces %>% before_parens(sep = "braces") %>% stringr::str_detect("\\}") %>% any() %>% expect_false()
  x_braces %>% inside_parens(sep = "braces") %>% stringr::str_detect("\\}") %>% any() %>% expect_false()
})



x_parens_proto   <- proto_split_parens(x_parens,   sep = "parens")
x_brackets_proto <- proto_split_parens(x_brackets, sep = "brackets")
x_braces_proto   <- proto_split_parens(x_braces,   sep = "braces")


test_that("The raw list elements all have length 2", {
  x_parens_proto   %>% purrr::map_int(length) %>% expect_setequal(2)
  x_brackets_proto %>% purrr::map_int(length) %>% expect_setequal(2)
  x_braces_proto   %>% purrr::map_int(length) %>% expect_setequal(2)
})

test_that("Wrong `sep` specifications trigger an error", {
  x_parens   %>% proto_split_parens(sep = "briquets") %>% expect_error()
  x_brackets %>% proto_split_parens(sep = "briquets") %>% expect_error()
  x_braces   %>% proto_split_parens(sep = "briquets") %>% expect_error()
})


