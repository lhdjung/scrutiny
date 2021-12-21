

# Example data:
pigs <- tibble::tribble(
  ~drone,           ~selfpilot,
  "0.09 (0.21)",    "0.19 (0.13)",
  "0.19 (0.28)",    "0.53 (0.10)",
  "0.62 (0.16)",    "0.50 (0.11)",
  "0.15 (0.35)",    "0.57 (0.16)",
)

pigs_tested <- pigs %>%
  split_by_parens()


test_that("The output is a tibble", {
  pigs_tested %>% tibble::is_tibble() %>% expect_true()
})


colnames_expected <- c("drone_x", "drone_sd", "selfpilot_x", "selfpilot_sd")

test_that("It has correct column names", {
  pigs_tested %>% colnames() %>% expect_equal(colnames_expected)
})


pigs_tested_transformed <- pigs %>%
  split_by_parens(.transform = TRUE)

x_expected  <- c(0.09, 0.19, 0.62, 0.15, 0.19, 0.53, 0.50, 0.57) %>%
  restore_zeros()

sd_expected <- c(0.21, 0.28, 0.16, 0.35, 0.13, 0.10, 0.11, 0.16) %>%
  restore_zeros()

test_that("It has correct values", {
  expect_equal(pigs_tested_transformed$x,  x_expected)
  expect_equal(pigs_tested_transformed$sd, sd_expected)
})


pigs_brackets <- pigs %>%
  dplyr::mutate(
    dplyr::across(everything(), stringr::str_replace, "\\(", "["),
    dplyr::across(everything(), stringr::str_replace, "\\)", "]")
  )

pigs_braces <- pigs %>%
  dplyr::mutate(
    dplyr::across(everything(), stringr::str_replace, "\\(", "{"),
    dplyr::across(everything(), stringr::str_replace, "\\)", "}")
  )


pigs_brackets_tested <- pigs_brackets %>%
  split_by_parens(.sep = "brackets")

pigs_braces_tested <- pigs_braces %>%
  split_by_parens(.sep = "braces")

test_that("The function works with square brackets as with parentheses", {
  (pigs_tested[1] == pigs_brackets_tested[1]) %>% all() %>% expect_true()
  (pigs_tested[2] == pigs_brackets_tested[2]) %>% all() %>% expect_true()
  (pigs_tested[3] == pigs_brackets_tested[3]) %>% all() %>% expect_true()
  (pigs_tested[4] == pigs_brackets_tested[4]) %>% all() %>% expect_true()
})

test_that("The function works with curly braces as with parentheses", {
  (pigs_tested[1] == pigs_braces_tested[1]) %>% all() %>% expect_true()
  (pigs_tested[2] == pigs_braces_tested[2]) %>% all() %>% expect_true()
  (pigs_tested[3] == pigs_braces_tested[3]) %>% all() %>% expect_true()
  (pigs_tested[4] == pigs_braces_tested[4]) %>% all() %>% expect_true()
})

test_that("The function works with curly braces as with square brackets", {
  (pigs_brackets_tested[1] == pigs_braces_tested[1]) %>% all() %>% expect_true()
  (pigs_brackets_tested[2] == pigs_braces_tested[2]) %>% all() %>% expect_true()
  (pigs_brackets_tested[3] == pigs_braces_tested[3]) %>% all() %>% expect_true()
  (pigs_brackets_tested[4] == pigs_braces_tested[4]) %>% all() %>% expect_true()
})



