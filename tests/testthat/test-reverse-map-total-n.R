

df1 <- tibble::tribble(
  ~x1,    ~x2,   ~n,
  "3.43", "5.28", 90,
  "2.97", "4.42", 103
)

df2 <- tibble::tibble(
  x1 = runif(150,  1, 100) %>% round(2) %>% as.character(),
  x2 = runif(150, 50, 150) %>% round(2) %>% as.character(),
  n  = runif(150, 20, 120) %>% round()
)

df1_tested <- grim_map_total_n(df1)
df2_tested <- grim_map_total_n(df2)

df1_rec <- reverse_map_total_n(df1_tested)
df2_rec <- reverse_map_total_n(df2_tested)


test_that("The reconstructed data frames are identical to the original ones", {
  expect_equal(df1, df1_rec)
  expect_equal(df2, df2_rec)
})
