test_that("the predicates return the expected output", {
  # Example test output:
  df1 <- grim_map(pigs1, digits_x = 2)
  df2 <- grim_map_seq(pigs1, digits_x = 2)
  df3 <- grim_map_total_n(tibble::tribble(
    ~x1,   ~x2,   ~n,
    3.43,  5.28,   90,
    2.97,  4.42,  103
  ), digits_x = 2)

  # All three tibbles are mapper output:
  df1 |> is_map_df() |> expect_true()
  df2 |> is_map_df() |> expect_true()
  df3 |> is_map_df() |> expect_true()

  # However, only `df1` is the output of a
  # basic mapper...
  df1 |> is_map_basic_df() |> expect_true()
  df2 |> is_map_basic_df() |> expect_false()
  df3 |> is_map_basic_df() |> expect_false()

  # ...only `df2` is the output of a
  # sequence mapper...
  df1 |> is_map_seq_df() |> expect_false()
  df2 |> is_map_seq_df() |> expect_true()
  df3 |> is_map_seq_df() |> expect_false()

  # ...and only `df3` is the output of a
  # total-n mapper:
  df1 |> is_map_total_n_df() |> expect_false()
  df2 |> is_map_total_n_df() |> expect_false()
  df3 |> is_map_total_n_df() |> expect_true()
})


# The patterns used to make the `scrutiny_` prefix optional, and the basic
# predicate rejected any class with `_map` in the middle, such as `map_check`.
test_that("the predicates recognize scrutiny's classes, and only those", {
  leaflet <- structure(
    tibble::tibble(a = 1),
    class = c("leaflet_map", "my_map_seq", "tbl_df", "tbl", "data.frame")
  )
  leaflet |> is_map_df()     |> expect_false()
  leaflet |> is_map_seq_df() |> expect_false()

  map_check <- function_map(
    function(y, n) TRUE,
    .reported = c("y", "n"),
    .name_test = "MAP_CHECK"
  )
  tibble::tibble(y = 1, n = 2) |> map_check() |> is_map_basic_df() |> expect_true()
})
