

test_that("A numeric argument for `x` leads to failure", {
  expect_error(grim(2.65, 30))
  expect_error(grim(924, 0))
})


test_that("Return values are Boolean", {
  expect_true(is.logical(grim("5.19", 28)))
  expect_true(is.logical(grim("0.00", 100)))
})


test_that("Correct values are returned (basic)", {
  expect_false(grim("5.19", 28)) # Should be FALSE
  expect_false(grim("5.20", 28)) # Should be FALSE
  expect_true( grim("5.21", 28)) # Should be TRUE
  expect_false(grim("5.22", 28)) # Should be FALSE
  expect_false(grim("5.23", 28)) # Should be FALSE
  expect_false(grim("5.24", 28)) # Should be FALSE
  expect_true( grim("5.25", 28)) # Should be TRUE
  expect_false(grim("5.26", 28)) # Should be FALSE
  expect_false(grim("5.27", 28)) # Should be FALSE
  expect_false(grim("5.28", 28)) # Should be FALSE
  expect_true( grim("5.29", 28)) # Should be TRUE
  expect_false(grim("5.30", 28)) # Should be FALSE
})


test_that("Correct values are returned (`items` argument)", {
  expect_true( grim("0.150", 120, items = 3)) # Should be TRUE
  expect_false(grim("0.151", 120, items = 3)) # Should be FALSE
  expect_false(grim("0.152", 120, items = 3)) # Should be FALSE
  expect_true( grim("0.153", 120, items = 3)) # Should be TRUE
  expect_false(grim("0.154", 120, items = 3)) # Should be FALSE
  expect_false(grim("0.155", 120, items = 3)) # Should be FALSE
  expect_true( grim("0.156", 120, items = 3)) # Should be TRUE
  expect_false(grim("0.157", 120, items = 3)) # Should be FALSE
  expect_true( grim("0.158", 120, items = 3)) # Should be TRUE
  expect_false(grim("0.159", 120, items = 3)) # Should be FALSE

  expect_true( grim("0.80", 28, 2)) # Should be TRUE
  expect_false(grim("0.81", 28, 2)) # Should be FALSE
  expect_true( grim("0.82", 28, 2)) # Should be TRUE
  expect_false(grim("0.83", 28, 2)) # Should be FALSE
  expect_true( grim("0.84", 28, 2)) # Should be TRUE
  expect_false(grim("0.85", 28, 2)) # Should be FALSE
  expect_true( grim("0.86", 28, 2)) # Should be TRUE
  expect_true( grim("0.87", 28, 2)) # Should be TRUE
  expect_true( grim("0.88", 28, 2)) # Should be TRUE
  expect_true( grim("0.89", 28, 2)) # Should be TRUE
})


test_that("Correct values are returned (`percent` argument)", {
  expect_false(grim("519", 28, percent = TRUE)) # Should be FALSE
  expect_false(grim("520", 28, percent = TRUE)) # Should be FALSE
  expect_true( grim("521", 28, percent = TRUE)) # Should be TRUE
  expect_false(grim("522", 28, percent = TRUE)) # Should be FALSE
  expect_false(grim("523", 28, percent = TRUE)) # Should be FALSE
  expect_false(grim("524", 28, percent = TRUE)) # Should be FALSE
  expect_true( grim("525", 28, percent = TRUE)) # Should be TRUE
  expect_false(grim("526", 28, percent = TRUE)) # Should be FALSE
  expect_false(grim("527", 28, percent = TRUE)) # Should be FALSE
  expect_false(grim("528", 28, percent = TRUE)) # Should be FALSE
  expect_true( grim("529", 28, percent = TRUE)) # Should be TRUE
  expect_false(grim("530", 28, percent = TRUE)) # Should be FALSE

  expect_true( grim("6",   50, percent = TRUE)) # Should be TRUE
  expect_false(grim("7",   50, percent = TRUE)) # Should be FALSE
  expect_true( grim("8",   50, percent = TRUE)) # Should be TRUE
  expect_false(grim("9",   50, percent = TRUE)) # Should be FALSE
  expect_true( grim("10",  50, percent = TRUE)) # Should be TRUE
  expect_false(grim("11",  50, percent = TRUE)) # Should be FALSE
  expect_true( grim("12",  50, percent = TRUE)) # Should be TRUE
  expect_false(grim("13",  50, percent = TRUE)) # Should be FALSE
  expect_true( grim("14",  50, percent = TRUE)) # Should be TRUE
  expect_false(grim("15",  50, percent = TRUE)) # Should be FALSE
  expect_true( grim("16",  50, percent = TRUE)) # Should be TRUE
})


test_that("`x` reappears as the names of the return values", {
  expect_true(hasName(grim("5.19", 28), "5.19"))
  expect_true(hasName(grim("5.20", 28), "5.20"))
  expect_true(hasName(grim("5.21", 28), "5.21"))
  expect_true(hasName(grim("5.22", 28), "5.22"))
  expect_true(hasName(grim("5.23", 28), "5.23"))
  expect_true(hasName(grim("5.24", 28), "5.24"))
  expect_true(hasName(grim("5.25", 28), "5.25"))
  expect_true(hasName(grim("5.26", 28), "5.26"))
  expect_true(hasName(grim("5.27", 28), "5.27"))
  expect_true(hasName(grim("5.28", 28), "5.28"))
  expect_true(hasName(grim("5.29", 28), "5.29"))
  expect_true(hasName(grim("5.30", 28), "5.30"))
})



# Example vectors for the test below:
x_length <- rnorm(1, 30, 3) %>%
  censor(25, 35) %>%
  round()

round_x_by <- rnorm(x_length, 2, 1) %>%
  censor(0, 3) %>%
  round()

x <- rnorm(x_length, 50, 20) %>%
  censor(10, 90) %>%
  round(round_x_by) %>%
  as.character()


test_that("Vectorization succeeds; there are as many outputs as inputs", {
  expect_equal(length(grim(x, 50)), x_length)
})


