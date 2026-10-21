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
  df1 |> grim_map_total_n(digits_x = 2, items_averaged = 1)                   |> audit_total_n() |> audit() |> expect_equal(df1_grim_exp)
  df1 |> grimmer_map_total_n(digits_x = 2, digits_sd = 2, items_averaged = 1) |> audit_total_n() |> audit() |> expect_equal(df1_grimmer_exp)
  df2 |> debit_map_total_n(digits_x = 2, digits_sd = 2)                       |> audit_total_n() |> audit() |> expect_equal(df2_debit_exp)
})
