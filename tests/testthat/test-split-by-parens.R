

# Example data:
pigs <- tibble::tribble(
  ~drone,           ~selfpilot,
  "0.09 (0.21)",    "0.19 (0.13)",
  "0.19 (0.28)",    "0.53 (0.10)",
  "0.62 (0.16)",    "0.50 (0.11)",
  "0.15 (0.35)",    "0.57 (0.16)",
)

pigs_tested <- split_by_parens(pigs)


test_that("The output is a tibble", {
  expect_s3_class(pigs_tested, "tbl_df")
})


colnames_expected <- c("drone_x", "drone_sd", "selfpilot_x", "selfpilot_sd")

test_that("It has correct column names", {
  expect_named(pigs_tested, colnames_expected)
})


pigs_tested_transformed <- split_by_parens(pigs, transform = TRUE)

x_expected  <- restore_zeros(c(0.09, 0.19, 0.62, 0.15, 0.19, 0.53, 0.50, 0.57))
sd_expected <- restore_zeros(c(0.21, 0.28, 0.16, 0.35, 0.13, 0.10, 0.11, 0.16))

test_that("It has correct values", {
  pigs_tested_transformed$x  %>% expect_equal(x_expected)
  pigs_tested_transformed$sd %>% expect_equal(sd_expected)
})


pigs_brackets <- pigs %>%
  dplyr::mutate(
    dplyr::across(everything(), function(x) stringr::str_replace(x, "\\(", "[")),
    dplyr::across(everything(), function(x) stringr::str_replace(x, "\\)", "]"))
  )

pigs_braces <- pigs %>%
  dplyr::mutate(
    dplyr::across(everything(), function(x) stringr::str_replace(x, "\\(", "{")),
    dplyr::across(everything(), function(x) stringr::str_replace(x, "\\)", "}"))
  )

pigs_brackets_tested <- split_by_parens(pigs_brackets, sep = "brackets")
pigs_braces_tested <- split_by_parens(pigs_braces, sep = "braces")

test_that("The function works with square brackets as with parentheses", {
  pigs_tested[1] %>% expect_equal(pigs_brackets_tested[1])
  pigs_tested[2] %>% expect_equal(pigs_brackets_tested[2])
  pigs_tested[3] %>% expect_equal(pigs_brackets_tested[3])
  pigs_tested[4] %>% expect_equal(pigs_brackets_tested[4])
})

test_that("The function works with curly braces as with parentheses", {
  pigs_tested[1] %>% expect_equal(pigs_braces_tested[1])
  pigs_tested[2] %>% expect_equal(pigs_braces_tested[2])
  pigs_tested[3] %>% expect_equal(pigs_braces_tested[3])
  pigs_tested[4] %>% expect_equal(pigs_braces_tested[4])
})

test_that("The function works with curly braces as with square brackets", {
  pigs_brackets_tested[1] %>% expect_equal(pigs_braces_tested[1])
  pigs_brackets_tested[2] %>% expect_equal(pigs_braces_tested[2])
  pigs_brackets_tested[3] %>% expect_equal(pigs_braces_tested[3])
  pigs_brackets_tested[4] %>% expect_equal(pigs_braces_tested[4])
})


test_that("named but non-formal arguments are an error", {
  # These should be caught by `rlang::check_dots_empty()` within
  # `check_new_args_without_dots()`:
  pigs %>% split_by_parens(abc = 5)         %>% expect_error()
  pigs %>% split_by_parens(no_arg = hello)  %>% expect_error()
})

test_that("using the dots, `...`, is an error", {
  pigs %>% split_by_parens(.transform = TRUE)                 %>% expect_error()
  pigs %>% split_by_parens(.transform = TRUE, .col1 = "mean") %>% expect_error()
  pigs %>% split_by_parens(wooh = 4)                          %>% expect_error()
})


pigs_wider <- pigs %>% dplyr::mutate(letters = letters[1:4])

test_that("non-`sep` columns are handled correctly with `check_sep = TRUE` (the default)", {
  expect_warning(out <- split_by_parens(pigs_wider))
  expect_equal(ncol(out), 5L)
})

test_that("non-`sep` columns are handled correctly with `check_sep = FALSE", {
  expect_warning(out <- split_by_parens(pigs_wider, check_sep = FALSE))
  expect_equal(ncol(out), 6L)
})


