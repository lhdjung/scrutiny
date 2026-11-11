# Example data ------------------------------------------------------------

# fmt: skip
df1 <- tibble::tribble(
  ~x1  , ~x2  , ~sd1 , ~sd2 , ~n ,
  3.43 , 5.28 , 1.09 , 2.12 , 70 ,
  2.97 , 4.42 , 0.43 , 1.65 , 65
)

# fmt: skip
df2 <- tibble::tribble(
  ~x1  , ~x2  , ~sd1 , ~sd2 , ~n ,
  0.30 , 0.28 , 0.17 , 0.10 , 70 ,
  0.41 , 0.39 , 0.09 , 0.15 , 65
)


# Expected output ---------------------------------------------------------

df1_grim_exp <- tibble::tibble(
  term = c(
    "hits_total",
    "hits_forth",
    "hits_back",
    "scenarios_total",
    "hit_rate"
  ),
  # The even total of 70 has 11 distinct splits, the odd one of 65 has 12:
  mean = c(2, 1.5, 0.5, 11.5, (1 / 11 + 3 / 12) / 2),
  sd = c(
    1.414213562373095145475,
    0.7071067811865475727373,
    0.7071067811865475727373,
    0.7071067811865475727373,
    (3 / 12 - 1 / 11) / sqrt(2)
  ),
  median = c(2, 1.5, 0.5, 11.5, (1 / 11 + 3 / 12) / 2),
  min = c(1, 1, 0, 11, 1 / 11),
  max = c(3, 2, 1, 12, 0.25),
  na_count = numeric(5),
  na_rate = numeric(5),
)

df1_grimmer_exp <- tibble::tibble(
  term = c(
    "hits_total",
    "hits_forth",
    "hits_back",
    "scenarios_total",
    "hit_rate"
  ),
  mean = c(0, 0, 0, 11.5, 0),
  sd = c(0, 0, 0, 0.7071067811865475727373, 0),
  median = c(0, 0, 0, 11.5, 0),
  min = c(0, 0, 0, 11, 0),
  max = c(0, 0, 0, 12, 0),
  na_count = numeric(5),
  na_rate = numeric(5),
)

df2_debit_exp <- tibble::tibble(
  term = c(
    "hits_total",
    "hits_forth",
    "hits_back",
    "scenarios_total",
    "hit_rate"
  ),
  mean = c(0, 0, 0, 11.5, 0),
  sd = c(0, 0, 0, 0.7071067811865475727373, 0),
  median = c(0, 0, 0, 11.5, 0),
  min = c(0, 0, 0, 11, 0),
  max = c(0, 0, 0, 12, 0),
  na_count = numeric(5),
  na_rate = numeric(5),
)


# Testing -----------------------------------------------------------------

test_that("`audit()` for `audit_total_n()` works correctly", {
  df1 |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer")                   |> audit()         |> audit() |> expect_equal(df1_grim_exp)
  df1 |> grimmer_map_total_n(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> audit()         |> audit() |> expect_equal(df1_grimmer_exp)
  df2 |> debit_map_total_n(digits_x = 2, digits_sd = 2, data_type = "integer")                       |> audit()         |> audit() |> expect_equal(df2_debit_exp)
})

test_that("undecidable splits count as neither hits nor scenarios", {
  # With one decimal place, a missing mean of 10 values or more would pass
  # whatever it is, so two decimal places it is:
  out <- tibble::tibble(x1 = c(NA, 5.2), x2 = 4, n = 30L) |>
    grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 0:1) |>
    audit()
  out$scenarios_total |> expect_equal(c(0L, 3L))
  out$hit_rate[1L]    |> expect_na()

  # A sample SD has no value for a group of 1, so those splits are left out:
  out <- tibble::tibble(x1 = 3, x2 = 4, sd1 = 0, sd2 = 0, n = 4L) |>
    grimmer_map_total_n(digits_x = 0, digits_sd = 0, items_averaged = 1, data_type = "integer", n_min = 1) |>
    audit()
  out$scenarios_total |> expect_equal(1L)
})


test_that("a direction whose rows were filtered out has unknown hits", {
  # `hits_back` was 0, and the totals and rate were those of "forth" alone:
  out <- tibble::tibble(x1 = c(3.43, 4.61), x2 = c(5.24, 4.19), n = c(70, 81)) |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer")
  audited <- out |> dplyr::filter(dir == "forth") |> audit()
  audited$hits_forth      |> expect_equal(audit(out)$hits_forth)
  audited$hits_back       |> expect_equal(c(NA_integer_, NA_integer_))
  audited$hits_total      |> expect_equal(c(NA_integer_, NA_integer_))
  audited$scenarios_total |> expect_equal(c(NA_integer_, NA_integer_))
  audited$hit_rate        |> expect_equal(c(NA_real_, NA_real_))
  out |> split(out$case) |> dplyr::bind_rows() |> audit() |> expect_equal(audit(out))
})


test_that("a total beyond the integer range stays as it is", {
  # `as.integer()` made it `NA`, with a warning:
  out <- tibble::tibble(x1 = 1.5, x2 = 2.5, n = 6e9) |>
    grim_map_total_n(digits_x = 1, items_averaged = 1, data_type = "integer", dispersion = 0:1) |>
    audit() |>
    expect_no_warning()
  out$n |> expect_equal(6e9)
  out$hits_total |> expect_type("integer")
  # Group sizes within the integer range, but not their sum:
  out <- tibble::tibble(x1 = 5.19, x2 = 4.2, n = 3e9) |>
    grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    audit() |>
    expect_no_warning()
  out$n |> expect_equal(3e9)
})


test_that("`audit()` and `reverse_map_total_n()` order string or factor cases as numbers", {
  # 12 cases, so that a string order would be 1, 10, 11, 12, 2, ...:
  dft <- tibble::tibble(x1 = rep(c(3.43, 2.97, 4.11), 4), x2 = rep(c(5.28, 4.42, 1.05), 4), n = 90:101)
  out <- dft |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 0:2)
  out_chr <- out |> dplyr::mutate(case = as.character(case))
  out_fct <- out |> dplyr::mutate(case = factor(case, levels = rev(unique(case))))
  out_chr |> audit()               |> expect_equal(audit(out))
  out_fct |> audit()               |> expect_equal(audit(out))
  out_chr |> reverse_map_total_n() |> expect_equal(reverse_map_total_n(out))
  out_fct |> reverse_map_total_n() |> expect_equal(reverse_map_total_n(out))
})
