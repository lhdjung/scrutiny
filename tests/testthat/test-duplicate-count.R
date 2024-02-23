
# Expected output ---------------------------------------------------------

iris_exp <- tibble::tibble(
  value = c(
    "setosa", "versicolor", "virginica", "0.2", "3", "1.5", "1.4", "1.3", "5.1",
    "5", "2.8", "3.2", "5.6", "2.3", "3.4", "1.8", "4.9", "5.7", "2.5", "3.1",
    "1.6", "5.5", "5.8", "6.3", "6.7", "2.9", "4.5", "4.8", "6.1", "2.7", "4.4",
    "5.4", "6", "6.4", "3.3", "3.5", "1", "4.6", "4.7", "2", "3.8", "1.2", "1.9",
    "0.3", "0.4", "5.2", "2.2", "2.4", "4", "1.7", "2.1", "5.9", "6.5", "6.9",
    "2.6", "3.6", "3.9", "4.2", "0.1", "6.2", "7.7", "3.7", "4.1", "1.1", "4.3",
    "5.3", "6.6", "6.8", "7.2", "7", "7.1", "7.3", "7.4", "7.6", "7.9", "0.5",
    "0.6"
  ),
  frequency = rep(
    c(
      50L, 29L, 27L, 25L, 21L, 20L, 17L, 14L, 13L, 12L, 11L, 10L,
      9L, 8L, 7L, 6L, 5L, 4L, 3L, 1L
    ),
    c(
      3L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 4L, 5L, 5L, 4L, 7L, 8L,
      6L, 8L, 5L, 5L, 8L
    )
  ),
  locations = c(
    "Species", "Species", "Species", "Petal.Width", "Sepal.Width, Petal.Length",
    "Petal.Length, Petal.Width", "Petal.Length, Petal.Width",
    "Petal.Length, Petal.Width", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Width", "Sepal.Width",
    "Sepal.Length, Petal.Length", "Sepal.Width, Petal.Width", "Sepal.Width",
    "Petal.Width", "Sepal.Length, Petal.Length", "Sepal.Length, Petal.Length",
    "Sepal.Width, Petal.Width", "Sepal.Width", "Petal.Length, Petal.Width",
    "Sepal.Length, Petal.Length", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Length, Petal.Length", "Sepal.Width",
    "Sepal.Length, Petal.Length", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Width",
    "Sepal.Length, Sepal.Width, Petal.Length", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Length, Petal.Length",
    "Sepal.Width, Petal.Length", "Sepal.Width, Petal.Length",
    "Petal.Length, Petal.Width", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Width, Petal.Width",
    "Sepal.Width, Petal.Length", "Petal.Length, Petal.Width",
    "Petal.Length, Petal.Width", "Petal.Width", "Petal.Width",
    "Sepal.Length, Petal.Length", "Sepal.Width, Petal.Width",
    "Sepal.Width, Petal.Width", "Sepal.Width, Petal.Length",
    "Petal.Length, Petal.Width", "Petal.Width", "Sepal.Length, Petal.Length",
    "Sepal.Length", "Sepal.Length, Petal.Length", "Sepal.Width",
    "Sepal.Width, Petal.Length", "Sepal.Width, Petal.Length",
    "Sepal.Width, Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Length",
    "Sepal.Width, Petal.Length", "Sepal.Width, Petal.Length",
    "Petal.Length, Petal.Width", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Length, Petal.Length", "Sepal.Length",
    "Sepal.Length", "Sepal.Length", "Sepal.Length", "Sepal.Length",
    "Sepal.Length", "Sepal.Length", "Sepal.Length", "Petal.Width", "Petal.Width"
  ),
  locations_n = rep(
    c(
      1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 3L, 2L, 1L, 2L,
      1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L
    ),
    c(
      4L, 6L, 2L, 2L, 2L, 3L, 1L, 5L, 1L, 3L, 1L, 1L, 12L, 2L, 5L,
      1L, 1L, 1L, 1L, 1L, 3L, 3L, 6L, 10L
    )
  ),
) %>%
  structure(class = c("scr_dup_count", "tbl_df", "tbl", "data.frame"))


mtcars_exp <- tibble::tibble(
  value = c(
    "0", "1", "4", "3", "8", "2", "6", "5", "275.8", "110", "175", "180", "3.07",
    "3.15", "3.92", "3.44", "10.4", "15.2", "15.5", "17.3", "19.2", "21", "21.4",
    "22.8", "30.4", "160", "167.6", "360", "66", "123", "150", "245", "2.76",
    "3.08", "3.73", "3.9", "4.08", "4.22", "3.57", "17.02", "18.9", "13.3",
    "14.3", "14.7", "15", "15.8", "16.4", "17.8", "18.1", "18.7", "19.7", "21.5",
    "24.4", "26", "27.3", "32.4", "33.9", "71.1", "75.7", "78.7", "79", "95.1",
    "108", "120.1", "120.3", "121", "140.8", "145", "146.7", "225", "258", "301",
    "304", "318", "350", "351", "400", "440", "460", "472", "52", "62", "65",
    "91", "93", "95", "97", "105", "109", "113", "205", "215", "230", "264",
    "335", "2.93", "3.21", "3.23", "3.54", "3.62", "3.69", "3.7", "3.77", "3.85",
    "4.11", "4.43", "4.93", "1.513", "1.615", "1.835", "1.935", "2.14", "2.2",
    "2.32", "2.465", "2.62", "2.77", "2.78", "2.875", "3.17", "3.19", "3.215",
    "3.435", "3.46", "3.52", "3.78", "3.84", "3.845", "4.07", "5.25", "5.345",
    "5.424", "14.5", "14.6", "15.41", "15.84", "16.46", "16.7", "16.87", "16.9",
    "17.05", "17.4", "17.42", "17.6", "17.82", "17.98", "18", "18.3", "18.52",
    "18.6", "18.61", "19.44", "19.47", "19.9", "20", "20.01", "20.22", "22.9"
  ),
  frequency = rep(
    c(37L, 34L, 33L, 19L, 15L, 10L, 8L, 5L, 3L, 2L, 1L),
    rep(c(1L, 8L, 25L, 117L), c(8L, 1L, 1L, 1L))
  ),
  locations = rep(
    c(
      "vs, am", "vs, am, carb", "cyl, gear, carb", "drat, gear, carb", "cyl, carb",
      "carb", "cyl, carb", "gear", "disp", "hp", "drat", "drat, wt", "drat", "wt",
      "mpg", "mpg, qsec", "mpg", "disp", "hp", "drat", "drat, wt", "drat", "wt",
      "qsec", "mpg", "disp", "hp", "drat", "wt", "qsec"
    ),
    c(
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 1L, 1L, 1L, 1L, 2L,
      2L, 5L, 3L, 4L, 2L, 1L, 3L, 1L, 2L, 16L, 23L, 15L, 12L, 25L,
      26L
    )
  ),
  locations_n = rep(
    c(2L, 3L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L),
    c(1L, 3L, 1L, 1L, 1L, 6L, 1L, 4L, 2L, 14L, 1L, 123L)
  ),
) %>%
  structure(class = c("scr_dup_count", "tbl_df", "tbl", "data.frame"))


iris_exp_ignore <- tibble::tibble(
  value = c(
    "setosa", "versicolor", "virginica", "0.2", "1.5", "1.4", "1.3", "5.1", "5",
    "2.8", "3.2", "5.6", "2.3", "3.4", "1.8", "4.9", "5.7", "2.5", "3.1", "1.6",
    "5.5", "5.8", "6.3", "6.7", "2.9", "4.5", "4.8", "6.1", "2.7", "4.4", "5.4",
    "6", "6.4", "3.3", "3.5", "1", "4.6", "4.7", "2", "3.8", "1.2", "1.9", "0.3",
    "0.4", "5.2", "2.2", "2.4", "4", "1.7", "2.1", "5.9", "6.5", "6.9", "2.6",
    "3.6", "3.9", "4.2", "0.1", "6.2", "7.7", "3.7", "4.1", "1.1", "4.3", "5.3",
    "6.6", "6.8", "7.2", "7", "7.1", "7.3", "7.4", "7.6", "7.9", "0.5", "0.6"
  ),
  frequency = rep(
    c(
      50L, 29L, 25L, 21L, 20L, 17L, 14L, 13L, 12L, 11L, 10L, 9L,
      8L, 7L, 6L, 5L, 4L, 3L, 1L
    ),
    c(
      3L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 4L, 5L, 5L, 4L, 7L, 8L, 6L,
      8L, 5L, 5L, 8L
    )
  ),
  locations = c(
    "Species", "Species", "Species", "Petal.Width", "Petal.Length, Petal.Width",
    "Petal.Length, Petal.Width", "Petal.Length, Petal.Width",
    "Sepal.Length, Petal.Length", "Sepal.Length, Petal.Length", "Sepal.Width",
    "Sepal.Width", "Sepal.Length, Petal.Length", "Sepal.Width, Petal.Width",
    "Sepal.Width", "Petal.Width", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Width, Petal.Width", "Sepal.Width",
    "Petal.Length, Petal.Width", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Width", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Length, Petal.Length", "Sepal.Width",
    "Sepal.Length, Sepal.Width, Petal.Length", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Length, Petal.Length",
    "Sepal.Width, Petal.Length", "Sepal.Width, Petal.Length",
    "Petal.Length, Petal.Width", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Width, Petal.Width",
    "Sepal.Width, Petal.Length", "Petal.Length, Petal.Width",
    "Petal.Length, Petal.Width", "Petal.Width", "Petal.Width",
    "Sepal.Length, Petal.Length", "Sepal.Width, Petal.Width",
    "Sepal.Width, Petal.Width", "Sepal.Width, Petal.Length",
    "Petal.Length, Petal.Width", "Petal.Width", "Sepal.Length, Petal.Length",
    "Sepal.Length", "Sepal.Length, Petal.Length", "Sepal.Width",
    "Sepal.Width, Petal.Length", "Sepal.Width, Petal.Length",
    "Sepal.Width, Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Length",
    "Sepal.Width, Petal.Length", "Sepal.Width, Petal.Length",
    "Petal.Length, Petal.Width", "Sepal.Length, Petal.Length",
    "Sepal.Length, Petal.Length", "Sepal.Length, Petal.Length", "Sepal.Length",
    "Sepal.Length", "Sepal.Length", "Sepal.Length", "Sepal.Length",
    "Sepal.Length", "Sepal.Length", "Sepal.Length", "Petal.Width", "Petal.Width"
  ),
  locations_n = rep(
    c(
      1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 3L, 2L, 1L, 2L,
      1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L
    ),
    c(
      4L, 5L, 2L, 2L, 2L, 3L, 1L, 5L, 1L, 3L, 1L, 1L, 12L, 2L, 5L,
      1L, 1L, 1L, 1L, 1L, 3L, 3L, 6L, 10L
    )
  ),
) %>%
  structure(class = c("scr_dup_count", "tbl_df", "tbl", "data.frame"))


mtcars_exp_ignore <- tibble::tibble(
  value = c(
    "0", "1", "4", "3", "8", "2", "6", "275.8", "110", "175", "180", "3.07",
    "3.15", "3.92", "3.44", "10.4", "15.2", "15.5", "17.3", "19.2", "21", "21.4",
    "22.8", "30.4", "160", "167.6", "360", "66", "123", "150", "245", "2.76",
    "3.08", "3.73", "3.9", "4.08", "4.22", "3.57", "17.02", "18.9", "13.3",
    "14.3", "14.7", "15", "15.8", "16.4", "17.8", "18.1", "18.7", "19.7", "21.5",
    "24.4", "26", "27.3", "32.4", "33.9", "71.1", "75.7", "78.7", "79", "95.1",
    "108", "120.1", "120.3", "121", "140.8", "145", "146.7", "225", "258", "301",
    "304", "318", "350", "351", "400", "440", "460", "472", "52", "62", "65",
    "91", "93", "95", "97", "105", "109", "113", "205", "215", "230", "264",
    "335", "2.93", "3.21", "3.23", "3.54", "3.62", "3.69", "3.7", "3.77", "3.85",
    "4.11", "4.43", "4.93", "1.513", "1.615", "1.835", "1.935", "2.14", "2.2",
    "2.32", "2.465", "2.62", "2.77", "2.78", "2.875", "3.17", "3.19", "3.215",
    "3.435", "3.46", "3.52", "3.78", "3.84", "3.845", "4.07", "5.25", "5.345",
    "5.424", "14.5", "14.6", "15.41", "15.84", "16.46", "16.7", "16.87", "16.9",
    "17.05", "17.4", "17.42", "17.6", "17.82", "17.98", "18", "18.3", "18.52",
    "18.6", "18.61", "19.44", "19.47", "19.9", "20", "20.01", "20.22", "22.9"
  ),
  frequency = rep(
    c(37L, 34L, 33L, 19L, 15L, 10L, 8L, 3L, 2L, 1L),
    rep(c(1L, 8L, 25L, 117L), c(7L, 1L, 1L, 1L))
  ),
  locations = rep(
    c(
      "vs, am", "vs, am, carb", "cyl, gear, carb", "drat, gear, carb", "cyl, carb",
      "carb", "cyl, carb", "disp", "hp", "drat", "drat, wt", "drat", "wt", "mpg",
      "mpg, qsec", "mpg", "disp", "hp", "drat", "drat, wt", "drat", "wt", "qsec",
      "mpg", "disp", "hp", "drat", "wt", "qsec"
    ),
    c(
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 1L, 1L, 1L, 1L, 2L, 2L,
      5L, 3L, 4L, 2L, 1L, 3L, 1L, 2L, 16L, 23L, 15L, 12L, 25L, 26L
    )
  ),
  locations_n = rep(
    c(2L, 3L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L),
    c(1L, 3L, 1L, 1L, 1L, 5L, 1L, 4L, 2L, 14L, 1L, 123L)
  ),
) %>%
  structure(class = c("scr_dup_count", "tbl_df", "tbl", "data.frame"))


vec_unnamed_exp <- tibble::tibble(
  value = c("3", "4", "5", "6", "7", "1", "2", "8", "9", "10"),
  frequency = rep(2:1, each = 5L),
) %>%
  structure(class = c("scr_dup_count", "tbl_df", "tbl", "data.frame"))


# Testing -----------------------------------------------------------------

test_that("`duplicate_count()` works correctly by default", {
  iris   %>% duplicate_count() %>% expect_equal(iris_exp)
  mtcars %>% duplicate_count() %>% expect_equal(mtcars_exp)
})

test_that("`duplicate_count()` works correctly with named lists", {
  iris   %>% as.list() %>% duplicate_count() %>% expect_equal(iris_exp)
  mtcars %>% as.list() %>% duplicate_count() %>% expect_equal(mtcars_exp)
})

test_that("`duplicate_count()` works correctly with `ignore` specified", {
  iris   %>% duplicate_count(ignore = 3) %>% expect_equal(iris_exp_ignore)
  mtcars %>% duplicate_count(ignore = 5) %>% expect_equal(mtcars_exp_ignore)
})

test_that("`duplicate_count()` works correctly with", {
  c(1:10, 3:7) %>% duplicate_count() %>% expect_equal(vec_unnamed_exp)
})
