
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



# Running old and new (= manufactured) functions --------------------------

out_grim_old1  <- grim_map(df_grim1)[1:3]
out_debit_old1 <- debit_map(df_debit1)[1:4]

out_grim_new1  <- grim_map_alt(df_grim1)
out_debit_new1 <- debit_map_alt(df_debit1)

out_grim_old2  <- grim_map(df_grim2)[1:3]
out_debit_old2 <- debit_map(df_debit2)[1:4]

out_grim_new2  <- grim_map_alt(df_grim2)
out_debit_new2 <- debit_map_alt(df_debit2)



# Testing -----------------------------------------------------------------

test_that("It works for GRIM", {
  out_grim_old1 %>% expect_equal(out_grim_new1)
  out_grim_old2 %>% expect_equal(out_grim_new2)
})

test_that("It works for DEBIT", {
  out_debit_old1 %>% expect_equal(out_debit_new1)
  out_debit_old2 %>% expect_equal(out_debit_new2)
})

test_that("Wrong `.reported` values throw an error", {
  grim_map_alt <- function_map(
    .fun = grim_scalar,
    .reported = c("x", "bla", "n"),
    .name_test = "GRIM",
    .name_class = "scr_grim_map"
  ) %>% expect_error()
})

