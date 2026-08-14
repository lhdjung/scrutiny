df1 <- tibble::tibble(
  y1 = 16:25,
  y2 = 26:35,
  n = seq(from = 12, to = 21, by = 1)
)


# First, create a mock consistency test, called SCHLIM. It's analogous to GRIM
# as implemented in scrutiny, which is also true for the function names:
schlim_scalar <- function(y, n) {
  # Note: `grim_scalar()` is not exported
  if (y / 3 > n) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Not needed below, but included for completeness:
schlim <- Vectorize(schlim_scalar)

# This will be the input function:
schlim_map <- function(data) {
  consistency <- purrr::map2_lgl(
    as.numeric(data$y),
    as.numeric(data$n),
    schlim_scalar
  )
  return(dplyr::mutate(data, consistency))
}

# Use the function factory:
schlim_map_total_n <- function_map_total_n(
  .fun = schlim_map,
  .reported = "y",
  .name_test = "SCHLIM",
  .name_class = "scrutiny_schlim_map_total_n"
)


# Apply the manufactured function to `df1`:
df1_tested <- schlim_map_total_n(df1)


# Conduct tests:
test_that("The manufactured function's output
          has the correct dimensions", {
  df1_tested |> dim() |> expect_equal(c(240, 7))
})


df1_tested_forth <- df1_tested |> dplyr::filter(dir == "forth")
df1_tested_back <- df1_tested |> dplyr::filter(dir == "back")

df1_tested_case1 <- df1_tested |> dplyr::filter(case == 1)
df1_tested_case2 <- df1_tested |> dplyr::filter(case == 2)
df1_tested_case3 <- df1_tested |> dplyr::filter(case == 3)


test_that("It has the correct dimensions when split by `dir`", {
  df1_tested_forth |> dim() |> expect_equal(c(120, 7))
  df1_tested_back  |> dim() |> expect_equal(c(120, 7))
})

test_that("It has the correct dimensions when split by `case`", {
  df1_tested_case1 |> dim() |> expect_equal(c(24, 7))
  df1_tested_case2 |> dim() |> expect_equal(c(24, 7))
  df1_tested_case3 |> dim() |> expect_equal(c(24, 7))
})


vals_exp_y <- c(16, 26, 16, 26, 16, 26)
vals_exp_n <- c(6, 6, 5, 7, 4, 8)

f <- FALSE
t <- TRUE
vals_exp_consistency <- c(f, t, t, t, t, t)

test_that("Judging by a small sample, it has correct values", {
  df1_tested$y[1:6] |> expect_equal(vals_exp_y)
  df1_tested$n[1:6] |> expect_equal(vals_exp_n)
  df1_tested$consistency[1:6] |> expect_equal(vals_exp_consistency)
})


test_that("the column order of `data` makes no difference", {
  # The key columns in reverse order. The "back" direction swaps the `y1` and
  # `y2` values by renaming and reordering columns, and the swap used to be
  # silently skipped -- returning "forth" results labeled "back" -- whenever
  # the key columns were not in the exact `y1, y2` order:
  df1_reversed <- df1[c("y2", "y1", "n")]
  expect_identical(schlim_map_total_n(df1_reversed), df1_tested)
})

test_that("the \"back\" direction really swaps the group pairings", {
  # `y1` values reappear as `y` in the "back" half's even rows (group 2), and
  # `y2` values in its odd rows (group 1) -- the reverse of "forth":
  expect_identical(unique(df1_tested_back$y[c(TRUE, FALSE)]), df1$y2)
  expect_identical(unique(df1_tested_back$y[c(FALSE, TRUE)]), df1$y1)
})


test_that("group suffixes are swapped even if the statistic's name has digits", {
  # The swap used to run `stringr::str_replace()` on the whole column name, so
  # it hit the wrong character as soon as the reported statistic was called
  # something like "t1", whose columns are `t11` and `t12`.
  t1_scalar <- function(t1, n, digits_t1) {
    abs(t1 * n - round(t1 * n)) < 10^-digits_t1
  }
  t1_map <- function_map(
    .fun = t1_scalar,
    .reported = c("t1", "n"),
    .args_by_row = "digits_t1",
    .name_test = "T1"
  )
  t1_map_total_n <- function_map_total_n(
    .fun = t1_map,
    .reported = "t1",
    .name_test = "T1"
  )
  out <- t1_map_total_n(
    tibble::tibble(t11 = 0.5, t12 = 0.25, n = 8),
    digits_t1 = 2
  )
  # In the "back" direction the two reported values change places, so the first
  # row of that half must carry `t12`'s value:
  out$t1[out$dir == "forth"][1L] |> expect_equal(0.5)
  out$t1[out$dir == "back"][1L]  |> expect_equal(0.25)
})
