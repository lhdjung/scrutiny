before <- rnorm(500, 25, 5) |>
  round(2) |>
  restore_zeros(width = 2)

inside <- rnorm(500, 25, 5) |>
  round(2) |>
  restore_zeros(width = 2)

x_parens <- paste0(before, " (", inside, ")")

x_brackets <- x_parens |>
  stringr::str_replace("\\(", "[") |>
  stringr::str_replace("\\)", "]")

x_braces <- x_parens |>
  stringr::str_replace("\\(", "{") |>
  stringr::str_replace("\\)", "}")


test_that("With parentheses, substrings are extracted
          from the expected positions", {
  x_parens |> before_parens(sep = "parens") |> expect_equal(before)
  x_parens |> inside_parens(sep = "parens") |> expect_equal(inside)
})

test_that("With parentheses, the separators are removed", {
  x_parens |> before_parens(sep = "parens") |> stringr::str_detect("\\(") |> any() |> expect_false()
  x_parens |> inside_parens(sep = "parens") |> stringr::str_detect("\\(") |> any() |> expect_false()
  x_parens |> before_parens(sep = "parens") |> stringr::str_detect("\\)") |> any() |> expect_false()
  x_parens |> inside_parens(sep = "parens") |> stringr::str_detect("\\)") |> any() |> expect_false()
})


test_that("With square brackets, substrings are extracted
          from the expected positions", {
  x_brackets |> before_parens(sep = "brackets") |> expect_equal(before)
  x_brackets |> inside_parens(sep = "brackets") |> expect_equal(inside)
})

test_that("With square brackets, the separators are removed", {
  x_brackets |> before_parens(sep = "brackets") |> stringr::str_detect("\\[") |> any() |> expect_false()
  x_brackets |> inside_parens(sep = "brackets") |> stringr::str_detect("\\[") |> any() |> expect_false()
  x_brackets |> before_parens(sep = "brackets") |> stringr::str_detect("\\]") |> any() |> expect_false()
  x_brackets |> inside_parens(sep = "brackets") |> stringr::str_detect("\\]") |> any() |> expect_false()
})


test_that("With curly braces, substrings are extracted
          from the expected positions", {
   x_braces |> before_parens(sep = "braces") |> expect_equal(before)
   x_braces |> inside_parens(sep = "braces") |> expect_equal(inside)
})

test_that("With curly braces, the separators are removed", {
  x_braces |> before_parens(sep = "braces") |> stringr::str_detect("\\{") |> any() |> expect_false()
  x_braces |> inside_parens(sep = "braces") |> stringr::str_detect("\\{") |> any() |> expect_false()
  x_braces |> before_parens(sep = "braces") |> stringr::str_detect("\\}") |> any() |> expect_false()
  x_braces |> inside_parens(sep = "braces") |> stringr::str_detect("\\}") |> any() |> expect_false()
})


x_parens_proto <- proto_split_parens(x_parens, sep = "parens")
x_brackets_proto <- proto_split_parens(x_brackets, sep = "brackets")
x_braces_proto <- proto_split_parens(x_braces, sep = "braces")


test_that("The raw output has one row per string and two columns", {
  x_parens_proto   |> dim() |> expect_equal(c(length(x_parens), 2L))
  x_brackets_proto |> dim() |> expect_equal(c(length(x_brackets), 2L))
  x_braces_proto   |> dim() |> expect_equal(c(length(x_braces), 2L))
})

test_that("Wrong `sep` specifications trigger an error", {
  x_parens   |> proto_split_parens(sep = "briquets") |> expect_error()
  x_brackets |> proto_split_parens(sep = "briquets") |> expect_error()
  x_braces   |> proto_split_parens(sep = "briquets") |> expect_error()
})

test_that("each string is split on its own, and `sep` is matched literally", {
  # Strings with more or fewer separators than the others used to shift parts
  # into other strings' rows:
  x <- c("1.5 (0.2)", "3.1", "4.2 (0.9) (n = 5)", NA)
  x |> before_parens() |> expect_equal(c("1.5", "3.1", "4.2", NA))
  x |> inside_parens() |> expect_equal(c("0.2", NA, "0.9", NA))
  "2.1 ( 0.3 )" |> inside_parens() |> expect_equal("0.3")
  "2.1 |0.3|" |> inside_parens(sep = c("|", "|")) |> expect_equal("0.3")
  "2.1 (0.3)" |> before_parens(sep = c("(", ")")) |> expect_equal("2.1")
})
