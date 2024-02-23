
basic1_exp <- c("20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30")

basic1_df_exp <- tibble::tibble(
  x = c("20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30")
)

basic2_exp <- c(
  "4.51", "4.52", "4.53", "4.54", "4.55", "4.56",
  "4.57", "4.58", "4.59", "4.60", "4.61"
)

basic2_df_exp <- tibble::tibble(
  x = c(
    "4.51", "4.52", "4.53", "4.54", "4.55", "4.56",
    "4.57", "4.58", "4.59", "4.60", "4.61"
  ),
)

with_out_max_exp <- list(
  c("70", "71", "72", "73", "74", "76", "77"),
  c(-5L, -4L, -3L, -2L, -1L, 1L, 2L)
)

with_out_max_df_exp <- tibble::tibble(
  x = c("70", "71", "72", "73", "74", "76", "77"),
  diff_var = c(-5L, -4L, -3L, -2L, -1L, 1L, 2L),
)

with_track_diff_var <- list(c("0.3", "0.9", "1.1", "1.2"), c(-3, 3, 5, 6))



# Testing -----------------------------------------------------------------

test_that("it works with the defaults", {
  seq_disperse(25)        %>% expect_equal(basic1_exp)
  seq_disperse_df(25)     %>% expect_equal(basic1_df_exp)
  seq_disperse("4.56")    %>% expect_equal(basic2_exp)
  seq_disperse_df("4.56") %>% expect_equal(basic2_df_exp)
})

test_that("it works when overriding some of the defaults", {
  seq_disperse(
    75, out_max = 77,
    include_reported = FALSE, track_diff_var = TRUE
  ) %>%
    expect_equal(with_out_max_exp)
  seq_disperse_df(
    75, .out_max = 77,
    .include_reported = FALSE, .track_diff_var = TRUE
  ) %>%
    expect_equal(with_out_max_df_exp)
  seq_disperse(
    from = 0.6, dispersion = c(3, 5, 6),
    track_diff_var = TRUE, include_reported = FALSE
  ) %>%
    expect_equal(with_track_diff_var)
})

