
# Manufactured functions --------------------------------------------------

# These two work, which is awesome! However, they can't be used as a basis for
# the respective `*_plot()` function because they lack a rounding class. This
# points at a way forward: (1) organize the rounding class stuff in a dedicated
# function which might even be exported; (2) call that function within
# `function_map()`

# I now solved it differently, at least for GRIM. This one works as a plot
# basis, but DEBIT doesn't because it doesn't create any of the extra columns
# that `debit_map()` does.

grim_map_alt <- function_map(
  .fun = grim_scalar,
  .reported = c("x", "n"),
  .name_test = "GRIM"
)

debit_map_alt <- function_map(
  .reported = c("x", "sd", "n"),
  .fun = debit_scalar,
  .name_test = "DEBIT"
)

grim_map_alt_renamed <- function_map(
  .fun = grim_scalar,
  .reported = c("x", "n"),
  .name_test = "GRIM",
  .name_key_result = "success"
)

debit_map_alt_renamed <- function_map(
  .reported = c("x", "sd", "n"),
  .fun = debit_scalar,
  .name_test = "DEBIT",
  .name_key_result = "success"
)



# Example data ------------------------------------------------------------

df_grim1  <- pigs1
df_debit1 <- pigs3

# Create this many random numbers per column:
n_dfs2 <- 150

df_grim2 <- tibble::tibble(
  x = runif(n_dfs2, 0, 10)   %>% round(2) %>% restore_zeros(width = 2),
  n = runif(n_dfs2, 40, 100) %>% round(0)
)

df_debit2 <- tibble::tibble(
  x  = runif(n_dfs2, 0.2, 0.7) %>% round(2) %>% restore_zeros(width = 2),
  sd = runif(n_dfs2, 0.1, 0.4) %>% round(2) %>% restore_zeros(width = 2),
  n  = runif(n_dfs2, 40, 100)  %>% round(0)
)

df_grim3 <- tibble::tibble(
  x = c(
    "7.22", "4.74", "5.23", "2.57", "6.77", "2.68", "7.01", "7.38", "3.14",
    "6.89", "5.00", "0.24"
  ),
  n = c(32, 25, 29, 24, 27, 28, 29, 26, 27, 31, 25, 28),
  success = c(
    TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE,
    FALSE, TRUE, FALSE
  ),
) %>%
  structure(
    class = c("scr_grim_map", "scr_rounding_up_or_down", "tbl_df", "tbl", "data.frame")
  )



# Running old and new (= manufactured) functions --------------------------

out_grim_old1  <- grim_map(df_grim1)[1:3]
out_debit_old1 <- debit_map(df_debit1)[1:4]

out_grim_new1  <- grim_map_alt(df_grim1)
out_debit_new1 <- debit_map_alt(df_debit1)

out_grim_old2  <- grim_map(df_grim2)[1:3]
out_debit_old2 <- debit_map(df_debit2)[1:4]

out_grim_new2  <- grim_map_alt(df_grim2)
out_debit_new2 <- debit_map_alt(df_debit2)


out_grim_old_renamed <- out_grim_old1 %>%
  dplyr::rename(success = consistency)

out_grim_new_renamed <- grim_map_alt_renamed(df_grim1)

out_debit_old_renamed <- out_debit_old1 %>%
  dplyr::rename(success = consistency)

out_debit_new_renamed <- debit_map_alt_renamed(df_debit1)



# Testing -----------------------------------------------------------------

test_that("It works for GRIM", {
  out_grim_old1 %>% expect_equal(out_grim_new1)
  out_grim_old2 %>% expect_equal(out_grim_new2)
})

test_that("It works for DEBIT", {
  out_debit_old1 %>% expect_equal(out_debit_new1)
  out_debit_old2 %>% expect_equal(out_debit_new2)
})

test_that("Renaming `\"consistency\"` via `.name_key_result` works", {
  out_grim_old_renamed  %>% expect_equal(out_grim_new_renamed)
  out_debit_old_renamed %>% expect_equal(out_debit_new_renamed)
})

test_that("Wrong `.reported` values throw an error", {
  function_map(
    .fun = grim_scalar,
    .reported = c("x", "success", "n"),
    .name_test = "GRIM"
  ) %>% expect_error()
})

