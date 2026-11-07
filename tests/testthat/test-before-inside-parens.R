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
  x_parens |> before_parens(delim = "parens") |> expect_equal(before)
  x_parens |> inside_parens(delim = "parens") |> expect_equal(inside)
})

test_that("With parentheses, the delimiters are removed", {
  x_parens |> before_parens(delim = "parens") |> stringr::str_detect("\\(") |> any() |> expect_false()
  x_parens |> inside_parens(delim = "parens") |> stringr::str_detect("\\(") |> any() |> expect_false()
  x_parens |> before_parens(delim = "parens") |> stringr::str_detect("\\)") |> any() |> expect_false()
  x_parens |> inside_parens(delim = "parens") |> stringr::str_detect("\\)") |> any() |> expect_false()
})


test_that("With square brackets, substrings are extracted
          from the expected positions", {
  x_brackets |> before_parens(delim = "brackets") |> expect_equal(before)
  x_brackets |> inside_parens(delim = "brackets") |> expect_equal(inside)
})

test_that("With square brackets, the delimiters are removed", {
  x_brackets |> before_parens(delim = "brackets") |> stringr::str_detect("\\[") |> any() |> expect_false()
  x_brackets |> inside_parens(delim = "brackets") |> stringr::str_detect("\\[") |> any() |> expect_false()
  x_brackets |> before_parens(delim = "brackets") |> stringr::str_detect("\\]") |> any() |> expect_false()
  x_brackets |> inside_parens(delim = "brackets") |> stringr::str_detect("\\]") |> any() |> expect_false()
})


test_that("With curly braces, substrings are extracted
          from the expected positions", {
  x_braces |> before_parens(delim = "braces") |> expect_equal(before)
  x_braces |> inside_parens(delim = "braces") |> expect_equal(inside)
})

test_that("With curly braces, the delimiters are removed", {
  x_braces |> before_parens(delim = "braces") |> stringr::str_detect("\\{") |> any() |> expect_false()
  x_braces |> inside_parens(delim = "braces") |> stringr::str_detect("\\{") |> any() |> expect_false()
  x_braces |> before_parens(delim = "braces") |> stringr::str_detect("\\}") |> any() |> expect_false()
  x_braces |> inside_parens(delim = "braces") |> stringr::str_detect("\\}") |> any() |> expect_false()
})


x_parens_proto   <- x_parens   |> proto_split_parens(delim = "parens") # fmt: skip
x_brackets_proto <- x_brackets |> proto_split_parens(delim = "brackets") # fmt: skip
x_braces_proto   <- x_braces   |> proto_split_parens(delim = "braces") # fmt: skip


test_that("The raw output has one row per string and two columns", {
  x_parens_proto   |> dim() |> expect_equal(c(length(x_parens), 2L))
  x_brackets_proto |> dim() |> expect_equal(c(length(x_brackets), 2L))
  x_braces_proto   |> dim() |> expect_equal(c(length(x_braces), 2L))
})

test_that("Wrong `delim` specifications trigger an error", {
  x_parens   |> proto_split_parens(delim = "briquets") |> expect_error()
  x_brackets |> proto_split_parens(delim = "briquets") |> expect_error()
  x_braces   |> proto_split_parens(delim = "briquets") |> expect_error()
})

test_that("each string is split on its own, and `delim` is matched literally", {
  # Strings with more or fewer delimiters than the others used to shift parts
  # into other strings' rows:
  x <- c("1.5 (0.2)", "3.1", "4.2 (0.9) (n = 5)", NA)
  x |> before_parens() |> expect_equal(c("1.5", "3.1", "4.2", NA))
  x |> inside_parens() |> expect_equal(c("0.2", NA, "0.9", NA))
  "2.1 ( 0.3 )" |> inside_parens() |> expect_equal("0.3")
  "2.1 |0.3|" |> inside_parens(delim = c("|", "|")) |> expect_equal("0.3")
  "2.1 (0.3)" |> before_parens(delim = c("(", ")")) |> expect_equal("2.1")
})


test_that("`delim` must not be missing", {
  "1 (2)" |> before_parens(delim = NA) |> expect_error("keyword or two strings")
  "1 (2)" |> inside_parens(delim = 1)  |> expect_error("keyword or two strings")
})


test_that("a missing `delim` element is shown as `NA`, not as a string", {
  "1 (2)" |> before_parens(delim = c("(", NA)) |> expect_error("`NA`")
})

test_that("an empty delimiter is an error", {
  "5 (1)" |> inside_parens(delim = c("", ")")) |> expect_error("keyword or two strings")
})


test_that("the old name `sep` still works, with a warning", {
  rlang::local_options(lifecycle_verbosity = "warning")
  "1 [2]" |> before_parens(sep = "brackets") |> expect_warning("delim")
  "1 [2]" |> before_parens(sep = "brackets") |> suppressWarnings() |> expect_equal("1")
  "1 [2]" |> inside_parens(sep = "brackets") |> suppressWarnings() |> expect_equal("2")
})
