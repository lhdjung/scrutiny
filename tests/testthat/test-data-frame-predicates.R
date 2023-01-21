
test_that("the predicates return the expected output", {
  # Example test output:
  df1 <- grim_map(pigs1)
  df2 <- grim_map_seq(pigs1)
  df3 <- grim_map_total_n(tibble::tribble(
    ~x1,    ~x2,   ~n,
    "3.43", "5.28", 90,
    "2.97", "4.42", 103
  ))

  # All three tibbles are mapper output:
  is_map_df(df1) %>% expect_true()
  is_map_df(df2) %>% expect_true()
  is_map_df(df3) %>% expect_true()

  # However, only `df1` is the output of a
  # basic mapper...
  is_map_basic_df(df1) %>% expect_true()
  is_map_basic_df(df2) %>% expect_false()
  is_map_basic_df(df3) %>% expect_false()

  # ...only `df2` is the output of a
  # sequence mapper...
  is_map_seq_df(df1) %>% expect_false()
  is_map_seq_df(df2) %>% expect_true()
  is_map_seq_df(df3) %>% expect_false()

  # ...and only `df3` is the output of a
  # total-n mapper:
  is_map_total_n_df(df1) %>% expect_false()
  is_map_total_n_df(df2) %>% expect_false()
  is_map_total_n_df(df3) %>% expect_true()
})

