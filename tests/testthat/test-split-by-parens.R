# Example data:
# fmt: skip
pigs <- tibble::tribble(
  ~drone        , ~selfpilot    ,
  "0.09 (0.21)" , "0.19 (0.13)" ,
  "0.19 (0.28)" , "0.53 (0.10)" ,
  "0.62 (0.16)" , "0.50 (0.11)" ,
  "0.15 (0.35)" , "0.57 (0.16)" ,
)

pigs_tested <- split_by_parens(pigs)


test_that("The output is a tibble", {
  pigs_tested |> expect_s3_class("tbl_df")
})


colnames_expected <- c("drone_x", "drone_sd", "selfpilot_x", "selfpilot_sd")

test_that("It has correct column names", {
  pigs_tested |> expect_named(colnames_expected)
})


pigs_tested_transformed <- split_by_parens(pigs, transform = TRUE)

x_expected <- restore_zeros(c(0.09, 0.19, 0.62, 0.15, 0.19, 0.53, 0.50, 0.57))
sd_expected <- restore_zeros(c(0.21, 0.28, 0.16, 0.35, 0.13, 0.10, 0.11, 0.16))

test_that("It has correct values", {
  pigs_tested_transformed$x  |> expect_equal(x_expected)
  pigs_tested_transformed$sd |> expect_equal(sd_expected)
})


pigs_brackets <- pigs |>
  dplyr::mutate(
    dplyr::across(everything(), function(x) {
      stringr::str_replace(x, "\\(", "[")
    }),
    dplyr::across(everything(), function(x) stringr::str_replace(x, "\\)", "]"))
  )

pigs_braces <- pigs |>
  dplyr::mutate(
    dplyr::across(everything(), function(x) {
      stringr::str_replace(x, "\\(", "{")
    }),
    dplyr::across(everything(), function(x) stringr::str_replace(x, "\\)", "}"))
  )

pigs_brackets_tested <- split_by_parens(pigs_brackets, sep = "brackets")
pigs_braces_tested <- split_by_parens(pigs_braces, sep = "braces")

test_that("The function works with square brackets as with parentheses", {
  pigs_tested[1] |> expect_equal(pigs_brackets_tested[1])
  pigs_tested[2] |> expect_equal(pigs_brackets_tested[2])
  pigs_tested[3] |> expect_equal(pigs_brackets_tested[3])
  pigs_tested[4] |> expect_equal(pigs_brackets_tested[4])
})

test_that("The function works with curly braces as with parentheses", {
  pigs_tested[1] |> expect_equal(pigs_braces_tested[1])
  pigs_tested[2] |> expect_equal(pigs_braces_tested[2])
  pigs_tested[3] |> expect_equal(pigs_braces_tested[3])
  pigs_tested[4] |> expect_equal(pigs_braces_tested[4])
})

test_that("The function works with curly braces as with square brackets", {
  pigs_brackets_tested[1] |> expect_equal(pigs_braces_tested[1])
  pigs_brackets_tested[2] |> expect_equal(pigs_braces_tested[2])
  pigs_brackets_tested[3] |> expect_equal(pigs_braces_tested[3])
  pigs_brackets_tested[4] |> expect_equal(pigs_braces_tested[4])
})


test_that("named but non-formal arguments are an error", {
  # These should be caught by `rlang::check_dots_empty()` within
  # `check_new_args_without_dots()`:
  pigs |> split_by_parens(abc = 5)         |> expect_error()
  pigs |> split_by_parens(no_arg = hello)  |> expect_error()
})

test_that("using the dots, `...`, is an error", {
  pigs |> split_by_parens(.transform = TRUE)                 |> expect_error()
  pigs |> split_by_parens(.transform = TRUE, .col1 = "mean") |> expect_error()
  pigs |> split_by_parens(wooh = 4)                          |> expect_error()
})


pigs_wider <- pigs |> dplyr::mutate(letters = letters[1:4])

test_that("non-`sep` columns are handled correctly with `check_sep = TRUE` (the default)", {
  expect_warning(out <- split_by_parens(pigs_wider))
  out |> ncol() |> expect_equal(5L)
})

test_that("non-`sep` columns are handled correctly with `check_sep = FALSE", {
  expect_warning(out <- split_by_parens(pigs_wider, check_sep = FALSE))
  out |> ncol() |> expect_equal(6L)
})

test_that("uneven separators, `NA`s, and column names ending on `end2` work", {
  tibble::tibble(a = c("1.2 (0.3) (n = 5)", "4.5 (0.6)"), b = c("1 (2)", NA)) |>
    split_by_parens() |>
    expect_equal(tibble::tibble(
      a_x = c("1.2", "4.5"), a_sd = c("0.3", "0.6"),
      b_x = c("1", NA), b_sd = c("2", NA)
    ))
  
  tibble::tibble(exp_sd = c("1 (2)", "3 (4)"), ctrl = c("5 (6)", "7 (8)")) |>
    split_by_parens(transform = TRUE) |>
    expect_equal(tibble::tibble(
      .origin = c("ctrl", "ctrl", "exp_sd", "exp_sd"),
      x = c("5", "7", "1", "3"),
      sd = c("6", "8", "2", "4")
    ))
})


# The legacy spellings of `sep`, such as `"("`, used to leave the warning
# unfinished: "It doesn't contain the `sep` elements, ."
test_that("the warning names the separators for every spelling of `sep`", {
  data <- tibble::tibble(a = c("1 (2)", "3 (4)"), b = c("x", "y"))
  for (sep in list("parens", "(", "\\(")) {
    data |> split_by_parens(sep = sep) |> expect_warning("i.e., parentheses")
  }
  data |> split_by_parens(sep = c("<", ">")) |> expect_warning('"<" and ">"')
})


# A split column used to overwrite an existing column of the same name, whose
# original values then replaced the split part in the output.
test_that("splitting never overwrites an existing column", {
  tibble::tibble(mean = c("1.5 (0.2)", "2 (0.5)"), mean_sd = c(9, 9))            |> split_by_parens() |> expect_error("`mean_sd` is already")
  tibble::tibble(exp = c("1.5 (0.2)", "2 (0.5)"), exp_sd = c("3 (0.1)", "4 (0.3)")) |> split_by_parens() |> expect_error("`exp_sd` is already")
  tibble::tibble(a = "1 (2)") |> split_by_parens(end1 = "z", end2 = "z") |> expect_error("must be different")
})
