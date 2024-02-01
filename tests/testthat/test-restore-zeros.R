

round_to <- rnorm(50, 2, 1) %>%
  censor(1, 4) %>%
  round()

numbers <- rnorm(50, 100, 30) %>%
  round(round_to) %>%
  restore_zeros(width = 4)


test_that("The number of decimal places checks out", {
  (decimal_places(numbers) == 4) %>% all() %>% expect_true()
})


test_that("The total number of characters checks out", {
  (stringr::str_length(numbers) - 5) %>% expect_equal(integer_places(numbers))
})


test_that("The `*_df()` variant produces correct results", {
  iris %>% restore_zeros_df() %>% expect_no_error()

  iris %>% restore_zeros_df(contains("Sepal")) %>% expect_no_error()

  iris %>%
    restore_zeros_df(contains("Sepal")) %>%
    purrr::map_chr(typeof) %>%
    unname() %>%
    expect_equal(c("character", "character", "double", "double", "integer"))

  iris %>%
    dplyr::select(1:4) %>%
    restore_zeros_df() %>%
    purrr::map_lgl(is.character) %>%
    all() %>%
    expect_true()

  iris %>%
    dplyr::select(5) %>%
    restore_zeros_df() %>%
    dplyr::pull(1) %>%
    expect_s3_class("factor")
})


test_that("the `check_decimals` argument works correctly", {
  iris %>%
    dplyr::mutate(Sepal.Length = trunc(Sepal.Length)) %>%
    restore_zeros_df(check_decimals = TRUE) %>%
    dplyr::pull(1) %>%
    expect_type("double")

  expect_warning(
    out <- iris %>%
      dplyr::mutate(Sepal.Length = trunc(Sepal.Length)) %>%
      restore_zeros_df(check_decimals = FALSE) %>%
      dplyr::pull(1)
  )
  expect_type(out, "character")
})


test_that("", {
  iris %>% restore_zeros_df(.check_decimals = TRUE) %>% expect_error()
  iris %>% restore_zeros_df(wooh = TRUE) %>% expect_error()
})



