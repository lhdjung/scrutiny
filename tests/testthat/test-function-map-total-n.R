

df1 <- tibble::tibble(
  y1 = 16:25,
  y2 = 26:35,
  n  = seq(from = 12, to = 21, by = 1)
)


# First, create a mock consistency test, called SCHLIM. It's analogous to GRIM
# as implemented in scrutiny, which is also true for the function names:
schlim_scalar <- function(y, n) {   # Note: `grim_scalar()` is not exported
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
  .name_class = "scr_schlim_map_total_n"
)


# Apply the manufactured function to `df1`:
df1_tested <- schlim_map_total_n(df1)


# Conduct tests:
test_that("The manufactured function's output
          has the correct dimensions", {
  df1_tested %>% dim() %>% expect_equal(c(240, 7))
})


df1_tested_forth <- df1_tested %>% dplyr::filter(dir == "forth")
df1_tested_back  <- df1_tested %>% dplyr::filter(dir == "back")

df1_tested_case1 <- df1_tested %>% dplyr::filter(case == 1)
df1_tested_case2 <- df1_tested %>% dplyr::filter(case == 2)
df1_tested_case3 <- df1_tested %>% dplyr::filter(case == 3)


test_that("It has the correct dimensions when split by `dir`", {
  df1_tested_forth %>% dim() %>% expect_equal(c(120, 7))
  df1_tested_back  %>% dim() %>% expect_equal(c(120, 7))
})

test_that("It has the correct dimensions when split by `case`", {
  df1_tested_case1 %>% dim() %>% expect_equal(c(24, 7))
  df1_tested_case2 %>% dim() %>% expect_equal(c(24, 7))
  df1_tested_case3 %>% dim() %>% expect_equal(c(24, 7))
})


vals_exp_y <- c(16, 26, 16, 26, 16, 26)
vals_exp_n <- c(6, 6, 5, 7, 4, 8)

f <- FALSE
t <- TRUE
vals_exp_consistency <- c(f, t, t, t, t, t)

test_that("Judging by a small sample, it has correct values", {
  df1_tested$y[1:6] %>% expect_equal(vals_exp_y)
  df1_tested$n[1:6] %>% expect_equal(vals_exp_n)
  df1_tested$consistency[1:6] %>% expect_equal(vals_exp_consistency)
})




