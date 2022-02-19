

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
  dplyr::select(
    x, consistency,
    rec_x_upper_rounded_up, rec_x_upper_rounded_down,
    rec_x_lower_rounded_up, rec_x_lower_rounded_down
  ) %>%
  dplyr::mutate(accord = dplyr::if_else(
      consistency,
      any(dplyr::near(
        as.numeric(x), c(
        rec_x_upper_rounded_up, rec_x_upper_rounded_down,
        rec_x_lower_rounded_up, rec_x_lower_rounded_down
      ))),
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



df4 <- df1 %>%
  grim_map(items = 2)

df4_cons_true <- df4$consistency[df4$consistency]

test_that("", {
  df4_cons_true %>% expect_length(6)
})


df5 <- df1 %>%
  grim_map(show_rec = TRUE)


test_that("`show_rec` increases the number of columns correctly", {
  df5 %>% ncol() %>% expect_equal(12)
})



df6 <- df1 %>%
  dplyr::rename(Mean = x, Sample_Size = n)

df6_grim <- df6 %>%
  grim_map(x = Mean, n = Sample_Size) %>%
  dplyr::mutate(Mean = NULL, Sample_Size = NULL)


test_that("`x` and `n` make the specified columns take on these roles", {
  df6_grim %>% expect_equal(df1_grim)
})


df7 <- df1 %>%
  grim_map(show_prob = TRUE)


test_that("`show_prob` adds a `prob` column", {
  "prob" %in% colnames(df7) %>% expect_true()
})


prob <- df7$prob
ratio_censored <- dplyr::if_else(df7$ratio < 0, 0, df7$ratio)

test_that("the probability of GRIM-inconsisteny is equal to the
          non-negative GRIM ratio", {
  prob %>% expect_equal(ratio_censored)
})



df8 <- tibble::tibble(
  x = df1$x,
  n40 = 40,
  n80 = 80
)

df8_n40_grim_up_or_down       <- grim_map(df8, n = n40, rounding = "up_or_down")
df8_n40_grim_up               <- grim_map(df8, n = n40, rounding = "up")
df8_n40_grim_down             <- grim_map(df8, n = n40, rounding = "down")
df8_n40_grim_even             <- grim_map(df8, n = n40, rounding = "even")
df8_n40_grim_ceiling_or_floor <- grim_map(df8, n = n40, rounding = "ceiling_or_floor")
df8_n40_grim_ceiling          <- grim_map(df8, n = n40, rounding = "ceiling")
df8_n40_grim_floor            <- grim_map(df8, n = n40, rounding = "floor")
df8_n40_grim_trunc            <- grim_map(df8, n = n40, rounding = "trunc")
df8_n40_grim_anti_trunc       <- grim_map(df8, n = n40, rounding = "anti_trunc")

df8_n80_grim_up_or_down       <- grim_map(df8, n = n80, rounding = "up_or_down")
df8_n80_grim_up               <- grim_map(df8, n = n80, rounding = "up")
df8_n80_grim_down             <- grim_map(df8, n = n80, rounding = "down")
df8_n80_grim_even             <- grim_map(df8, n = n80, rounding = "even")
df8_n80_grim_ceiling_or_floor <- grim_map(df8, n = n80, rounding = "ceiling_or_floor")
df8_n80_grim_ceiling          <- grim_map(df8, n = n80, rounding = "ceiling")
df8_n80_grim_floor            <- grim_map(df8, n = n80, rounding = "floor")
df8_n80_grim_trunc            <- grim_map(df8, n = n80, rounding = "trunc")
df8_n80_grim_anti_trunc       <- grim_map(df8, n = n80, rounding = "anti_trunc")


# Function for creating expected logical vectors. Make a list with
# `df8_n40_grim_up_or_down` and all other like it (with `n40`), then run
# `purrr::map(format_consistency_results)` on that list. Copy the resulting
# vectors into the matrix-like scheme below. When finished, do the same with the
# `n80` objects.
format_consistency_results <- function(df) {
  out <- df$consistency %>%
    purrr::map_chr(paste0, ", ") %>%
    stringr::str_flatten() %>%
    stringr::str_remove(", $") %>%
    stringr::str_replace_all("TRUE", "T") %>%
    stringr::str_replace_all("FALSE", "F")

  paste0("c(", out, ")")
}


df8_n40_grim_up_or_down_exp       <- c(T, F, T, T, T, T, F, T, F, F, T, F)
df8_n40_grim_up_exp               <- c(F, F, T, F, F, T, F, T, F, F, T, F)
df8_n40_grim_down_exp             <- c(T, F, F, T, T, F, F, F, F, F, T, F)
df8_n40_grim_even_exp             <- c(T, F, F, F, F, F, F, T, F, F, T, F)
df8_n40_grim_ceiling_or_floor_exp <- c(T, F, T, T, T, T, F, T, F, F, T, F)
df8_n40_grim_ceiling_exp          <- c(F, F, T, F, F, T, F, T, F, F, T, F)
df8_n40_grim_floor_exp            <- c(T, F, F, T, T, F, F, F, F, F, T, F)
df8_n40_grim_trunc_exp            <- c(T, F, F, T, T, F, F, F, F, F, T, F)
df8_n40_grim_anti_trunc_exp       <- c(F, F, T, F, F, T, T, T, F, F, F, F)


test_that("rounding specifications lead to the expected consistency
          results in the corner case of n = 40", {
  df8_n40_grim_up_or_down       $consistency %>% expect_equal(df8_n40_grim_up_or_down_exp       )
  df8_n40_grim_up               $consistency %>% expect_equal(df8_n40_grim_up_exp               )
  df8_n40_grim_down             $consistency %>% expect_equal(df8_n40_grim_down_exp             )
  df8_n40_grim_even             $consistency %>% expect_equal(df8_n40_grim_even_exp             )
  df8_n40_grim_ceiling_or_floor $consistency %>% expect_equal(df8_n40_grim_ceiling_or_floor_exp )
  df8_n40_grim_ceiling          $consistency %>% expect_equal(df8_n40_grim_ceiling_exp          )
  df8_n40_grim_floor            $consistency %>% expect_equal(df8_n40_grim_floor_exp            )
  df8_n40_grim_trunc            $consistency %>% expect_equal(df8_n40_grim_trunc_exp            )
  df8_n40_grim_anti_trunc       $consistency %>% expect_equal(df8_n40_grim_anti_trunc_exp       )
})


df8_n80_grim_up_or_down_exp       <- c(T, T, T, T, T, T, T, T, T, T, T, T)
df8_n80_grim_up_exp               <- c(F, T, T, F, F, T, T, T, T, T, T, T)
df8_n80_grim_down_exp             <- c(T, T, F, T, T, F, T, F, T, T, T, T)
df8_n80_grim_even_exp             <- c(T, T, F, F, F, F, T, T, T, T, T, T)
df8_n80_grim_ceiling_or_floor_exp <- c(T, T, T, T, T, T, T, T, T, T, T, T)
df8_n80_grim_ceiling_exp          <- c(T, T, T, T, T, T, F, T, T, T, T, T)
df8_n80_grim_floor_exp            <- c(T, F, T, T, T, T, T, T, F, F, T, F)
df8_n80_grim_trunc_exp            <- c(T, F, T, T, T, T, T, T, F, F, T, F)
df8_n80_grim_anti_trunc_exp       <- c(T, T, T, T, T, T, T, T, T, T, F, T)


test_that("rounding specifications lead to the expected consistency
          results in the corner case of n = 80", {
  df8_n80_grim_up_or_down       $consistency %>% expect_equal(df8_n80_grim_up_or_down_exp       )
  df8_n80_grim_up               $consistency %>% expect_equal(df8_n80_grim_up_exp               )
  df8_n80_grim_down             $consistency %>% expect_equal(df8_n80_grim_down_exp             )
  df8_n80_grim_even             $consistency %>% expect_equal(df8_n80_grim_even_exp             )
  df8_n80_grim_ceiling_or_floor $consistency %>% expect_equal(df8_n80_grim_ceiling_or_floor_exp )
  df8_n80_grim_ceiling          $consistency %>% expect_equal(df8_n80_grim_ceiling_exp          )
  df8_n80_grim_floor            $consistency %>% expect_equal(df8_n80_grim_floor_exp            )
  df8_n80_grim_trunc            $consistency %>% expect_equal(df8_n80_grim_trunc_exp            )
  df8_n80_grim_anti_trunc       $consistency %>% expect_equal(df8_n80_grim_anti_trunc_exp       )
})



df9_up_1 <- grim_map(df1, rounding = "up_from", threshold = 1)
df9_up_9 <- grim_map(df1, rounding = "up_from", threshold = 9)

df9_up_1_exp <- c(T, F, F, F, F, T, F, F, F, F, T, F)
df9_up_9_exp <- c(F, F, F, F, T, F, F, T, T, F, T, F)

test_that("the minimum of `threshold` yields expected results", {
  df9_up_1$consistency %>% expect_equal(df9_up_1_exp)
  df9_up_9$consistency %>% expect_equal(df9_up_9_exp)
})


df9_down_1 <- grim_map(df1, rounding = "down_from", threshold = 1)
df9_down_9 <- grim_map(df1, rounding = "down_from", threshold = 9)

df9_down_1_exp <- c(F, F, F, F, T, F, F, T, T, F, T, F)
df9_down_9_exp <- c(T, F, F, F, F, T, F, F, F, F, T, F)

test_that("the maximum of `threshold` yields expected results", {
  df9_down_1$consistency %>% expect_equal(df9_down_1_exp)
  df9_down_9$consistency %>% expect_equal(df9_down_9_exp)
})




# Errors ------------------------------------------------------------------

df10 <- df1 %>%
  dplyr::mutate(items = 2)

df11 <- df1 %>%
  dplyr::rename(Snout = x)

df11_exp <- grim_map(df1)

df12 <- df1 %>%
  dplyr::rename(Sample_Size = n)

df12_exp <- grim_map(df1)


test_that("expectations related to various individual
          error messages hold", {
  df1  %>% grim_map(items = 1:3) %>% expect_error()
  df10 %>% grim_map(items = 3) %>% expect_error()
  df11 %>% grim_map(x = Snout) %>% expect_equal(df11_exp)
  df11 %>% grim_map(x = Mouth) %>% expect_error()
  df12 %>% grim_map(n = Sample_Size) %>% expect_equal(df12_exp)
  df12 %>% grim_map(n = Count_Pigs) %>% expect_error()
})


df13 <- df1 %>%
  dplyr::mutate(girth = 30, mirth = 50, birth = 70)

df13_exp <- grim_map(df1)

test_that("`extra = 0` drops all extra columns", {
  df13 %>% grim_map(extra = 0) %>% expect_equal(df13_exp)
})








