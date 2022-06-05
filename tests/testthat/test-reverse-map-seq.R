

# Define example data -----------------------------------------------------

df_schlim <- tibble::tibble(y = 16:100, n = 3:87)
df_grim <- pigs1
df_debit <- pigs3


# Create a mock `*_seq()` function ----------------------------------------

schlim_scalar <- function(y, n) {
  y <- as.numeric(y)
  n <- as.numeric(n)
  y / 3 > n
}

schlim_map <- function(data) {
  y <- data$y
  n <- data$n
  consistency <- purrr::map2_lgl(y, n, schlim_scalar)
  out <- tibble::tibble(y, n, consistency)
  out <- add_class(out, "scr_schlim_map")  # See section "S3 classes" below
  out
}

schlim_map_seq <- function_map_seq(
  .fun = schlim_map,
  .reported = c("y", "n"),
  .name_test = "SCHLIM",
  .name_class = "scr_schlim_map_seq"
)


# Apply the reversal ------------------------------------------------------

df_schlim_rec <- df_schlim %>%
  schlim_map_seq(include_consistent = TRUE) %>%
  reverse_map_seq()

df_grim_rec <- df_grim %>%
  grim_map_seq(include_consistent = TRUE) %>%
  reverse_map_seq()

df_debit_rec <- df_debit %>%
  debit_map_seq(include_consistent = TRUE) %>%
  reverse_map_seq()


# Test for equality with the original -------------------------------------

test_that("It works with SCHLIM (toy test)", {
  df_schlim %>% expect_equal(df_schlim_rec)
})

test_that("It works with GRIM", {
  df_grim %>% expect_equal(df_grim_rec)
})

test_that("It works with DEBIT", {
  df_debit %>% expect_equal(df_debit_rec)
})


