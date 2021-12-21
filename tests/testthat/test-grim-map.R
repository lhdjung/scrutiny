

df1 <- pigs1

df1_grim_up_or_down       <- grim_map(df1, rounding = "up_or_down")
df1_grim_up               <- grim_map(df1, rounding = "up")
df1_grim_down             <- grim_map(df1, rounding = "down")
df1_grim_even             <- grim_map(df1, rounding = "even")
df1_grim_ceiling_or_floor <- grim_map(df1, rounding = "ceiling_or_floor")
df1_grim_ceiling          <- grim_map(df1, rounding = "ceiling")
df1_grim_floor            <- grim_map(df1, rounding = "floor")
df1_grim_trunc            <- grim_map(df1, rounding = "trunc")
df1_grim_anti_trunc       <- grim_map(df1, rounding = "anti_trunc")


df1_grim <- grim_map(df1)

test_that("A tibble is returned", {
  expect_true(is.data.frame(df1_grim))
  expect_true(inherits(df1_grim, "tbl_df"))
  expect_true(inherits(df1_grim, "tbl"))
})



test_that("It has the correct function-general class", {
  expect_true(inherits(df1_grim, "scr_grim_map"))
})

test_that("It has the correct rounding-specific class", {
  expect_true( inherits( df1_grim_up_or_down,       "scr_rounding_up_or_down"))
  expect_true( inherits( df1_grim_up,               "scr_rounding_up"))
  expect_true( inherits( df1_grim_down,             "scr_rounding_down"))
  expect_true( inherits( df1_grim_ceiling_or_floor, "scr_rounding_ceiling_or_floor"))
  expect_true( inherits( df1_grim_ceiling,          "scr_rounding_ceiling"))
  expect_true( inherits( df1_grim_floor,            "scr_rounding_floor"))
  expect_true( inherits( df1_grim_trunc,            "scr_rounding_trunc"))
  expect_true( inherits( df1_grim_anti_trunc,       "scr_rounding_anti_trunc"))
})


consistency_expected <- c(T, F, F, F, F, T, F, T, F, F, T, F)

test_that("`consistency` has the correct values", {
  df1_grim$consistency %>% expect_equal(consistency_expected)
})



df2 <- df1 %>%
  dplyr::mutate(n = n * 100)

df2_grim <- grim_map(df2, show_prob = TRUE)

test_that("`prob` is zero if `ratio` is negative", {
  (df2_grim$prob == 0) %>% all() %>% expect_true()
})

test_that("`prob` is greater than `ratio` if `ratio` is negative", {
  (df2_grim$prob > df2_grim$ratio) %>% all() %>% expect_true()
})



x <- rnorm(500, 65, 15) %>%
  censor(30, 90) %>%
  round() %>%
  as.character()

n <- rnorm(500, 50, 20) %>%
  censor(20, 90) %>%
  round()

df3 <- tibble::tibble(x, n)
df3_percent_true <- grim_map(df3, percent = TRUE, show_rec = TRUE) %>%
  suppressMessages()
df3_percent_false <- grim_map(df3, show_rec = TRUE)

all_percent_ratios_greater <-
  (df3_percent_true$ratio > df3_percent_false$ratio) %>%
  all()

test_that(
  "The GRIM ratio is always greater with `percent = TRUE` than without it", {
    all_percent_ratios_greater %>% expect_true()
})


df3_true_accord <- df3_percent_true %>%
  dplyr::select(x, consistency, rec_x_upper_rounded, rec_x_lower_rounded) %>%
  dplyr::mutate(accord = dplyr::if_else(
      consistency,
      any(dplyr::near(as.numeric(x), rec_x_upper_rounded, rec_x_lower_rounded)),
      FALSE
    ))

accord <- (df3_true_accord$consistency == df3_true_accord$accord) %>%
  all()


test_that(glue::glue(
  "The stated consistency accords with what can be reconstructed \\
  from the numbers presented"
), {
  accord %>% expect_true()
})


