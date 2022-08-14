

test_that("A numeric argument for `x` leads to failure", {
  expect_error(grim(2.65, 30))
  expect_error(grim(924, 0))
})


test_that("Return values are Boolean", {
  expect_true(is.logical(grim("5.19", 28)))
  expect_true(is.logical(grim("0.00", 100)))
})


vec1 <- seq_endpoint(5.19, 5.3)

vec1_tested <- vec1 %>%
  grim(28) %>%
  unname()

vec1_expected <- c(F, F, T, F, F, F, T, F, F, F, T, F)


test_that("Correct values are returned (basic)", {
  expect_equal(vec1_tested, vec1_expected)
})


vec2 <- seq_endpoint(0.150, 0.159)

vec2_tested <- vec2 %>%
  grim(120, items = 3) %>%
  unname()

vec2_expected <- c(T, F, F, T, F, F, T, F, T, F)


vec3 <- seq_endpoint(0.80, 0.89)

vec3_tested <- vec3 %>%
  grim(28, items = 2) %>%
  unname()

vec3_expected <- c(T, F, T, F, T, F, T, T, T, T)


test_that("Correct values are returned (`items` argument)", {
  expect_equal(vec2_tested, vec2_expected)
  expect_equal(vec3_tested, vec3_expected)
})


vec4 <- seq_endpoint(519, 530)

vec4_tested <- vec4 %>%
  grim(28, percent = TRUE) %>%
  unname()

vec4_expected <- c(F, F, T, F, F, F, T, F, F, F, T, F)

vec5 <- seq_endpoint(6, 16)

vec5_tested <- vec5 %>%
  grim(50, percent = TRUE) %>%
  unname()

vec5_expected <- c(T, F, T, F, T, F, T, F, T, F, T)


test_that("Correct values are returned (`percent` argument)", {
  expect_equal(vec4_tested, vec4_expected)
  expect_equal(vec5_tested, vec5_expected)
})


vec <- seq_endpoint(5, 5.99)

vec_all_right_name <- vec %>%
  grim(28) %>%
  hasName(vec) %>%
  all()


test_that("The `x` vector reappears as the names of the return values", {
  vec_all_right_name %>% expect_true()
})



# Example vectors for the test below:
x_length <- rnorm(1, 30, 3) %>%
  censor(25, 35) %>%
  round()

what_to_round_x_by <- rnorm(x_length, 2, 1) %>%
  censor(0, 3) %>%
  round()

x <- rnorm(x_length, 50, 20) %>%
  censor(10, 90) %>%
  round(what_to_round_x_by) %>%
  as.character()


test_that("There are as many outputs as inputs", {
  grim(x, 50) %>% length() %>% expect_equal(x_length)
})




# Comparing GRIM implementations ------------------------------------------

# # NOTE: All of the below has been outcommented in order to avoid an rsprite2
# dependency -- at least for now.

# compare_grim_implementations <- function(x_seq_string, n = 40,
#                                          filter = TRUE) {
#   out <- tibble::tibble(x = x_seq_string, n) %>%
#     grim_map() %>%
#     dplyr::select(-ratio) %>%
#     dplyr::mutate(
#       consistency_old = purrr::map2_lgl(
#         as.numeric(x), n,
#         rsprite2::GRIM_test, m_prec = 2
#       )
#     )
#   if (filter) {
#     out <- dplyr::filter(out, consistency != consistency_old)
#   }
#   return(out)
# }
#
#
#
# n_test_cases <- 5000
#
#
# seq_random <- runif(n_test_cases, 0, 50)
# seq_random <- seq_random %>%
#   round_up(2) %>%
#   restore_zeros(width = 2)
#
#
# n_random <- rnorm(n_test_cases, 50, 10)
# n_random <- round(n_random, 0)
#
# df_80 <- compare_grim_implementations(seq_random, n = 80)
# df_80
#
#
#
# seq_random0 <- as.numeric(seq_random) - trunc(as.numeric(seq_random))
# seq_random0 <- restore_zeros(seq_random0, width = 2)
#
#
# df_40_0 <- compare_grim_implementations(seq_random0, n = 40)
# df_80_0 <- compare_grim_implementations(seq_random0, n = 80)
#
# df_40_0
# df_80_0
#
#
# n_random <- rnorm(n_test_cases, 50, 10)
# n_random <- round(n_random, 0)
#
# df_random <- compare_grim_implementations(seq_random, n = n_random)
# df_random
#
#
# # All of these data frames only list cases of disagreement between the two GRIM
# # implementations, so these two agree completely if and only if the data frames
# # don't list any cases; i.e., they have no rows:
# test_that("Both GRIM implementations agree completely", {
#   df_80     %>% nrow() %>% expect_equal(0)
#   df_40_0   %>% nrow() %>% expect_equal(0)
#   df_80_0   %>% nrow() %>% expect_equal(0)
#   df_random %>% nrow() %>% expect_equal(0)
# })

