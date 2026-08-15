df1 <- disperse(n = 20, dispersion = 0:5)

test_that("The function returns a tibble", {
  df1 |> expect_s3_class("tbl_df")
})

test_that("It has the right dimensions", {
  df1 |> dim() |> expect_equal(c(12, 2))
})

test_that("It starts with the right values", {
  df1$n[1] |> expect_equal(20)
  df1$n_change[1] |> expect_equal(0)
})

test_that("It ends with the right values", {
  df1$n[12] |> expect_equal(25)
  df1$n_change[12] |> expect_equal(5)
})


df2 <- disperse(0, dispersion = 0:5, n_min = 1)

test_that("`n_min` controls the dimensions correctly", {
  df2 |> dim() |> expect_equal(c(0, 2))
})


test_that("`dispersion` must consist of whole numbers", {
  # `n_change` used to be truncated toward zero by `as.integer()`, so a
  # fractional step left `n` and `n_change` describing different things.
  disperse(n = 10, dispersion = c(0.5, 1.5)) |> expect_error("whole numbers")
  disperse(n = 10, dispersion = 1.5) |> expect_error("whole numbers")
})

test_that("`n` and `n_change` agree", {
  out <- disperse(n = 10)
  (out$n - 10L) |> expect_equal(out$n_change)
})


test_that("`n` is integer, as it is in the mappers", {
  # The mappers store `n` as integer because a sample size is a whole number.
  # The dispersion helpers that feed them, and the reverse function that reads
  # their output back, returned doubles -- so the column changed type on the
  # way in and back out again.
  disperse(20)$n |> expect_type("integer")
  disperse2(c(25, 26))$n |> expect_type("integer")
  disperse_total(40)$n |> expect_type("integer")
  disperse_total(51)$n |> expect_type("integer")
  disperse(20)$n_change |> expect_type("integer")

  reverse_map_total_n(
    grim_map_total_n(tibble::tibble(x1 = 4.52, x2 = 5.23, n = 40L),
                     digits_x = 2)
  )$n |>
    expect_type("integer")

  # The values themselves are unchanged:
  disperse(20)$n |> expect_equal(c(20L, 20L, 19L, 21L, 18L, 22L, 17L, 23L,
                                   16L, 24L, 15L, 25L))
})
