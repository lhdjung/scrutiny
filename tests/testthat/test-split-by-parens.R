# Example data:
# fmt: skip
pigs <- tibble::tribble(
  ~drone        , ~selfpilot    ,
  "0.09 (0.21)" , "0.19 (0.13)" ,
  "0.19 (0.28)" , "0.53 (0.10)" ,
  "0.62 (0.16)" , "0.50 (0.11)" ,
  "0.15 (0.35)" , "0.57 (0.16)" ,
)

pigs_tested <- pigs |> split_by_parens()


test_that("The output is a tibble", {
  pigs_tested |> expect_s3_class("tbl_df")
})


colnames_expected <- c("drone_x", "drone_sd", "selfpilot_x", "selfpilot_sd")

test_that("It has correct column names", {
  pigs_tested |> expect_named(colnames_expected)
})


pigs_tested_transformed <- pigs |> split_by_parens(transform = TRUE)

x_expected  <- c(0.09, 0.19, 0.62, 0.15, 0.19, 0.53, 0.50, 0.57) |> restore_zeros() # fmt: skip
sd_expected <- c(0.21, 0.28, 0.16, 0.35, 0.13, 0.10, 0.11, 0.16) |> restore_zeros() # fmt: skip

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

pigs_brackets_tested <- pigs_brackets |> split_by_parens(delim = "brackets") # fmt: skip
pigs_braces_tested   <- pigs_braces   |> split_by_parens(delim = "braces") # fmt: skip

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
  pigs |> split_by_parens(abc = 5)         |> expect_error()
  pigs |> split_by_parens(no_arg = hello)  |> expect_error()
})

test_that("using the dots, `...`, is an error", {
  pigs |> split_by_parens(.transform = TRUE)                 |> expect_error()
  pigs |> split_by_parens(.transform = TRUE, .col1 = "mean") |> expect_error()
  pigs |> split_by_parens(wooh = 4)                          |> expect_error()
})


pigs_wider <- pigs |> dplyr::mutate(letters = letters[1:4])

test_that("non-`delim` columns are handled correctly with `check_delim = TRUE` (the default)", {
  pigs_wider |> split_by_parens() |> ncol() |> expect_equal(5L) |> expect_warning()
})

test_that("non-`delim` columns are handled correctly with `check_delim = FALSE", {
  pigs_wider |> split_by_parens(check_delim = FALSE) |> ncol() |> expect_equal(6L) |> expect_warning()
})

test_that("uneven delimiters, `NA`s, and column names ending on `end2` work", {
  tibble::tibble(a = c("1.2 (0.3) (n = 5)", "4.5 (0.6)"), b = c("1 (2)", NA)) |>
    split_by_parens() |>
    expect_equal(tibble::tibble(
      a_x = c("1.2", "4.5"), a_sd = c("0.3", "0.6"),
      b_x = c("1", NA), b_sd = c("2", NA)
    ))

  tibble::tibble(exp_sd = c("1 (2)", "3 (4)"), ctrl = c("5 (6)", "7 (8)")) |>
    split_by_parens(transform = TRUE) |>
    expect_equal(tibble::tibble(
      .origin = c("exp_sd", "exp_sd", "ctrl", "ctrl"),
      x = c("1", "3", "5", "7"),
      sd = c("2", "4", "6", "8")
    ))
})


# The legacy spellings of `delim`, such as `"("`, used to leave the warning
# unfinished: "It doesn't contain the `delim` elements, ."
test_that("the warning names the delimiters for every spelling of `delim`", {
  data <- tibble::tibble(a = c("1 (2)", "3 (4)"), b = c("x", "y"))
  for (delim in list("parens", "(", "\\(")) {
    data |> split_by_parens(delim = delim) |> expect_warning("i.e., parentheses")
  }
  data |> split_by_parens(delim = c("<", ">")) |> expect_warning('"<" and ">"')
})


# A split column used to overwrite an existing column of the same name, whose
# original values then replaced the split part in the output.
test_that("splitting never overwrites an existing column", {
  tibble::tibble(mean = c("1.5 (0.2)", "2 (0.5)"), mean_sd = c(9, 9))            |> split_by_parens() |> expect_error("`mean_sd` is already")
  tibble::tibble(exp = c("1.5 (0.2)", "2 (0.5)"), exp_sd = c("3 (0.1)", "4 (0.3)")) |> split_by_parens() |> expect_error("`exp_sd` is already")
  tibble::tibble(a = "1 (2)") |> split_by_parens(end1 = "z", end2 = "z") |> expect_error("must be different")
})

test_that("`end1` and `end2` must be single strings", {
  # A vector failed in dplyr, and `NA` was pasted in as "a_NA":
  tibble::tibble(a = "1 (2)") |> split_by_parens(end1 = c("x", "y")) |> expect_error("single string")
  tibble::tibble(a = "1 (2)") |> split_by_parens(end2 = NA)          |> expect_error("single string")
})


# On 0 rows, `check_delim = TRUE` warned and returned the input unsplit, while
# `check_delim = FALSE` split it, and warned that it couldn't:
test_that("0-row character columns are split either way, without a warning", {
  empty <- tibble::tibble(a = character(0))
  empty |> split_by_parens()                  |> colnames() |> expect_equal(c("a_x", "a_sd")) |> expect_no_warning()
  empty |> split_by_parens(check_delim = FALSE) |> colnames() |> expect_equal(c("a_x", "a_sd")) |> expect_no_warning()
})


test_that("`split_by_parens()` works on grouped tibbles", {
  grouped <- tibble::tibble(a = c("1 (2.5)", "3 (4)"), b = 1:2) |> dplyr::group_by(b)
  out <- grouped |> split_by_parens() |> suppressWarnings()
  out$a_x |> expect_equal(c("1", "3"))
  out |> dplyr::is_grouped_df() |> expect_false()
  # Grouping plays no role, so a grouping column is split like any other:
  by_a <- grouped |> dplyr::group_by(a) |> split_by_parens() |> suppressWarnings()
  by_a$a_x |> expect_equal(c("1", "3"))
  by_a |> dplyr::is_grouped_df() |> expect_false()
})


test_that("`transform = TRUE` keeps the columns in their order", {
  # The output used to be sorted alphabetically by `.origin`:
  tibble::tibble(z = "1 (2)", a = "3 (4)") |>
    split_by_parens(transform = TRUE) |>
    dplyr::pull(.origin) |>
    expect_equal(c("z", "a"))
})


test_that("the flags of `split_by_parens()` must be flags", {
  df <- tibble::tibble(a = "1.2 (3.4)")
  df |> split_by_parens(keep = NA)        |> expect_error("`keep` must be `TRUE` or `FALSE`")
  df |> split_by_parens(check_delim = "no") |> expect_error("`check_delim` must be `TRUE` or `FALSE`")
  df |> split_by_parens(transform = 1)    |> expect_error("`transform` must be `TRUE` or `FALSE`")
})

test_that("a column named `delim` is split, not taken for the argument", {
  df <- tibble::tibble(aug = c("0.09 (0.21)", "0.19 (0.28)"), delim = c("0.19 (0.13)", "0.53 (0.10)"))
  out <- df |> split_by_parens()
  out |> colnames() |> expect_equal(c("aug_x", "aug_sd", "delim_x", "delim_sd"))
  out$aug_sd        |> expect_equal(c("0.21", "0.28"))
  out$delim_x       |> expect_equal(c("0.19", "0.53"))
})

test_that("the `*_df()` functions name the vector function for a vector", {
  # These failed with "no applicable method for 'ungroup'":
  c(1.5, 2) |> decimal_places_df() |> expect_error("Did you mean `decimal_places()`", fixed = TRUE)
  "1 (2)"   |> split_by_parens()   |> expect_error("Did you mean `before_parens()`", fixed = TRUE)
  1         |> restore_zeros_df()  |> expect_error("Did you mean `restore_zeros()`", fixed = TRUE)
})

test_that("a data frame without columns comes back as it is", {
  # `.before = 1L` had no column to refer to:
  tibble::tibble() |> split_by_parens()   |> expect_identical(tibble::tibble())
  tibble::tibble() |> decimal_places_df() |> expect_identical(tibble::tibble())
  tibble::tibble() |> restore_zeros_df()  |> expect_identical(tibble::tibble())
})

test_that("`cols` can't rename columns in the `*_df()` functions", {
  # A rename failed with a vctrs error, added a column, or split into columns
  # named after the new name while warning that it was not split:
  data <- tibble::tibble(a = c("1.5", "2.25"), b = c("1 (2)", "3 (4)"))
  data |> decimal_places_df(cols = c(new = a)) |> expect_error("Can't rename")
  data |> restore_zeros_df(cols = c(new = a))  |> expect_error("Can't rename")
  data |> split_by_parens(cols = c(new = b))   |> expect_error("Can't rename")
})

test_that("a column named `_` is no name clash", {
  # With no column to split, `paste0()` made the new name `"_"`:
  data <- tibble::tibble("_" = c("a", "b"))
  data |> split_by_parens() |> expect_warning("not split")
  data |> split_by_parens() |> suppressWarnings() |> colnames() |> expect_equal("_")
})


test_that("name clashes and empty input get the package's own errors and shapes", {
  # Two new columns of the same name failed inside `dplyr::across()`:
  tibble::tibble(a = c("1 (2)", "3 (4)"), a_x = c("5 (6)", "7 (8)")) |>
    split_by_parens(end1 = "x_y", end2 = "y") |>
    expect_error("Splitting would overwrite existing columns")

  # The transformed data have an `.origin` column of their own:
  tibble::tibble(a = "1 (2)") |>
    split_by_parens(end1 = ".origin", end2 = "sd", transform = TRUE) |>
    expect_error("can't be \".origin\"")

  # No column to stack was a subsetting error:
  tibble::tibble() |>
    split_by_parens(transform = TRUE) |>
    expect_identical(tibble::tibble(.origin = character(0), x = character(0), sd = character(0)))

  # Repeated column names failed inside `dplyr::select()`:
  data.frame(a = "1 (2)", a = "3 (4)", check.names = FALSE) |>
    split_by_parens() |>
    expect_error("must have unique names")
})


test_that("the old names `sep` and `check_sep` still work, with a warning", {
  rlang::local_options(lifecycle_verbosity = "warning")
  df <- tibble::tibble(a = c("1 [2]", "3 [4]"))
  expected <- df |> split_by_parens(delim = "brackets", check_delim = FALSE)
  df |> split_by_parens(sep = "brackets")                     |> expect_warning("delim")
  df |> split_by_parens(delim = "brackets", check_sep = FALSE) |> expect_warning("check_delim")
  df |> split_by_parens(sep = "brackets")                     |> suppressWarnings() |> expect_equal(expected)
  df |> split_by_parens(delim = "brackets", check_sep = FALSE) |> suppressWarnings() |> expect_equal(expected)
  df |> split_by_parens(sep = "brackets", delim = "brackets")  |> expect_error("can't both be given")
})


test_that("`split_by_parens(check_delim = FALSE)` says a column was split anyway", {
  df <- tibble::tibble(a = c("1 (2", "3 (4)"))
  df |> split_by_parens(check_delim = FALSE) |> expect_warning("1 column was split anyway: `a`", fixed = TRUE)
  df |> split_by_parens(check_delim = FALSE) |> expect_warning("Not every value has the `delim` elements", fixed = TRUE)
  df |> split_by_parens()                    |> expect_warning("1 column was not split: `a`", fixed = TRUE)
})
