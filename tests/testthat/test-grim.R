

test_that("`digits_x` missing leads to failure", {
  expect_error(grim(2.65, 30))
  expect_error(grim(924, 0))
})


test_that("Return values are Boolean", {
  expect_type(grim(5.19, 28, digits_x = 2), "logical")
  expect_type(grim(0.00, 100, digits_x = 2), "logical")
})


vec1 <- as.numeric(seq_endpoint(5.19, 5.3))

vec1_tested <- vec1 %>%
  grim(28, digits_x = 2) %>%
  unname()

t <- TRUE
f <- FALSE

vec1_expected <- c(f, f, t, f, f, f, t, f, f, f, t, f)


test_that("Correct values are returned (basic)", {
  expect_equal(vec1_tested, vec1_expected)
})


vec2 <- as.numeric(seq_endpoint(0.150, 0.159))

vec2_tested <- vec2 %>%
  grim(120, digits_x = 3, items = 3) %>%
  unname()

vec2_expected <- c(t, f, f, t, f, f, t, f, t, f)


vec3 <- as.numeric(seq_endpoint(0.80, 0.89))

vec3_tested <- vec3 %>%
  grim(28, digits_x = 2, items = 2) %>%
  unname()

vec3_expected <- c(t, f, t, f, t, f, t, t, t, t)


test_that("Correct values are returned (`items` argument)", {
  expect_equal(vec2_tested, vec2_expected)
  expect_equal(vec3_tested, vec3_expected)
})


vec4 <- as.numeric(seq_endpoint(519, 530))

vec4_tested <- vec4 %>%
  grim(28, digits_x = 0, percent = TRUE) %>%
  unname()

vec4_expected <- c(f, f, t, f, f, f, t, f, f, f, t, f)

vec5 <- as.numeric(seq_endpoint(6, 16))

vec5_tested <- vec5 %>%
  grim(50, digits_x = 0, percent = TRUE) %>%
  unname()

vec5_expected <- c(t, f, t, f, t, f, t, f, t, f, t)


test_that("Correct values are returned (`percent` argument)", {
  expect_equal(vec4_tested, vec4_expected)
  expect_equal(vec5_tested, vec5_expected)
})


vec <- as.numeric(seq_endpoint(5, 5.99))



test_that("The number of outputs matches the number of inputs", {
  expect_length(grim(vec, 28, digits_x = 2), length(vec))
})



# Example vectors for the test below:
x_length <- rnorm(1, 30, 3) %>%
  censor(25, 35) %>%
  round()

x <- rnorm(x_length, 50, 20) %>%
  censor(10, 90) %>%
  round(2)


test_that("There are as many outputs as inputs", {
  grim(x, 50, digits_x = 2) %>% expect_length(x_length)
})


