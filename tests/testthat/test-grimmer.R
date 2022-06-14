
# Generating 22223 values in the first step leaves 20000 values with exactly 2
# decimal places in the second. There are the first 20000 values greater than 1
# that have exactly two decimal places and where the second decimal place is not
# zero (so it counts as a decimal place even without a string transformation):
df1_mean <- seq(1, length.out = 22223, by = 0.01)
df1_mean <- df1_mean[decimal_places(df1_mean) == 2]

length(df1_mean)

# Random `n` values with the same number as the mean values, truncated because
# they can only be whole numbers:
df1_n <- runif(length(df1_mean), 10, 150)
df1_n <- trunc(df1_n)

# Create an example data frame:
df1 <- tibble::tibble(
  n    = df1_n,
  mean = as.character(df1_mean),
  sd   = as.character(round(as.numeric(mean) * (2 / 5), 2))
)

# The same data frame but with a different name for the `sd` column; this is
# just due to a naming difference between the two functions:
df2 <- df1 %>%
  dplyr::rename(SD = sd) %>%
  dplyr::mutate(mean = as.numeric(mean), SD = as.numeric(SD))

# Helper that turns the two functions' string output into `TRUE` if consistent
# and `FALSE` if inconsistent:
as_logical_consistency <- function(x) {
  !stringr::str_detect(x, "inconsistent")
}

# Run the tests:
out1 <- purrr::pmap_chr(df1, grimmer_scalar)
out2 <- purrr::pmap_chr(df2, aGrimmer)

# Translate to Boolean:
out1_lgl <- out1 %>% as_logical_consistency()
out2_lgl <- out2 %>% as_logical_consistency()


test_that("Both agree on consistency per se; i.e., Boolean values", {
  out1_lgl %>% expect_equal(out2_lgl)
})

test_that("Both agree on the reasons for (in)consistency, as well", {
  out1 %>% expect_equal(out2)
})


df_out <- tibble::tibble(
  df1, out1_lgl, out2_lgl, out1, out2
)

# The problem seems to be limited to cases where `out1` is consistent and `out2`
# is GRIMMER-inconsistent (but not GRIM-inconsistent). It is heavily
# concentrated on cases where `sd` has exactly 1 decimal place. There are few
# such `sd` values with no decimal places and, as of yet, none with 2:
df_disagree <- df_out %>%
  dplyr::filter(out1 != out2) %>%
  dplyr::mutate(digits_sd = decimal_places(sd), .after = sd)


df_disagree


