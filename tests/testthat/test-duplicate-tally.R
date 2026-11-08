test_that("`duplicate_tally()` does not count missing values as matches", {
  # `x == x[i]` is `NA` against a missing value, and indexing by `NA` returns an
  # element, so every count was off by the number of missing values:
  c(1, 1, NA) |> duplicate_tally() |> purrr::pluck("value_n") |> expect_equal(c(2L, 2L, NA))
  out <- tibble::tibble(a = c(1, 2), b = c(1, NA)) |> duplicate_tally()
  out$a_n |> expect_equal(c(2L, 1L))
  out$b_n |> expect_equal(c(2L, NA))
})


test_that("`duplicate_tally()` works without rows and on matrices", {
  numeric(0) |> duplicate_tally() |> colnames() |> expect_equal(c("value", "value_n"))
  pigs4[0, ] |> duplicate_tally() |> colnames() |> expect_equal(c("snout", "snout_n", "tail", "tail_n", "wings", "wings_n"))
  c(1, 1, 2, 3) |>
    matrix(nrow = 2) |>
    duplicate_tally() |>
    expect_equal(tibble::tibble(col1 = c("1", "1"), col1_n = 2L, col2 = c("2", "3"), col2_n = 1L), ignore_attr = c("class", "scrutiny"))
})


# `as.character(100000)` is `"1e+05"`, so an integer and a double `100000` were
# no duplicates:
test_that("`duplicate_tally()` compares numbers as written out in full", {
  out <- tibble::tibble(a = c(100000L, 2L), b = c(100000, 0.0001)) |> duplicate_tally()
  out$a_n |> expect_equal(c(2L, 1L))
  out$b_n |> expect_equal(c(2L, 1L))
  tibble::tibble(a = 100000L, b = 100000) |> duplicate_tally(ignore = 1e5) |> dplyr::pull(a_n) |> expect_na()
})


test_that("a matrix keeps its column names", {
  m <- as.matrix(pigs4[1:3, ])
  m |> duplicate_tally() |> colnames() |> expect_equal(c(rbind(colnames(pigs4), paste0(colnames(pigs4), "_n"))))
  m |> unname() |> duplicate_tally() |> colnames() |> expect_equal(c("col1", "col1_n", "col2", "col2_n", "col3", "col3_n"))
  # Names that a list couldn't have are treated as in a list:
  m2 <- matrix(c(1, 2, 1, 3), 2)
  m2 |> `colnames<-`(c("a", "a")) |> duplicate_tally() |> expect_error("unique names")
  m2 |> `colnames<-`(c("a", NA))  |> duplicate_tally() |> colnames() |> expect_equal(c("col1", "col1_n", "col2", "col2_n"))
  m2 |> `colnames<-`(c("a", ""))  |> duplicate_tally() |> colnames() |> expect_equal(c("col1", "col1_n", "col2", "col2_n"))
})
