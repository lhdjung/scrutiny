# Source of the exported `pigs1`-`pigs5` datasets. See R/data-doc.R for their
# documentation.

# GRIM for means:
# fmt: skip
pigs1 <- tibble::tribble(
  ~x,      ~n,
  7.22,    32,
  4.74,    25,
  5.23,    29,
  2.57,    24,
  6.77,    27,
  2.68,    28,
  7.01,    29,
  7.38,    26,
  3.14,    27,
  6.89,    31,
  5.00,    25,
  0.24,    28,
)


# GRIM for percentages:
# fmt: skip
pigs2 <- tibble::tribble(
  ~x,   ~n,
  67.4, 150,
  54.2, 150,
  54.0, 150,
  69.8, 150,
  68.1, 150,
  55.4, 150,
)


# DEBIT:
# fmt: skip
pigs3 <- tibble::tribble(
  ~x,       ~sd,    ~n,
  0.53,     0.50,   1683,
  0.44,     0.50,   1683,
  0.77,     0.42,   1683,
  0.19,     0.35,   1683,
  0.34,     0.47,   1683,
  0.93,     0.25,   1683,
  0.12,     0.33,   1683,
)


# fmt: skip
pigs4 <- tibble::tribble(
  ~snout,  ~tail,  ~wings,
  4.73,    6.88,   6.09,
  8.13,    7.33,   8.27,
  4.22,    5.17,   4.40,
  4.22,    7.57,   5.92,
  5.17,    8.13,   5.17,
)


# GRIMMER:
# fmt: skip
pigs5 <- tibble::tribble(
  ~x,      ~sd,     ~n,
  7.22,    5.30,    38,
  4.74,    6.55,    31,
  5.23,    2.55,    35,
  2.57,    2.57,    30,
  6.77,    2.18,    33,
  2.68,    2.59,    34,
  7.01,    6.68,    35,
  7.38,    3.65,    32,
  3.14,    5.32,    33,
  6.89,    4.18,    37,
  5.00,    2.18,    31,
  0.24,    6.43,    34,
)


# Save data:
usethis::use_data(
  pigs1,
  pigs2,
  pigs3,
  pigs4,
  pigs5,
  overwrite = TRUE
)
