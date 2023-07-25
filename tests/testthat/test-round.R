
x <- runif(100000, min = 1900, max = 2100)
x_up   <- x %>% round_up(2)   %>% trunc_reverse()
x_down <- x %>% round_down(2) %>% trunc_reverse()

# Avoid whole numbers that will be rounded up, artificially leading to a
# difference of 1:
x_up <- x_up[x_up != 0]
x_down <- x_down[x_down != 0]



# Threshold 5 (via the wrappers) ------------------------------------------

test_that("`round_up()` works correctly", {
  expect_equal(
    x_up,
    x_up %>% trunc_reverse() %>% round_up(2)
  )
})

test_that("`round_down()` works correctly", {
  expect_equal(
    x_down,
    x_down %>% trunc_reverse() %>% round_down(2)
  )
})


# Threshold 3 -------------------------------------------------------------

x_up_from_3   <- x %>% round_up_from(2, threshold = 3)
x_down_from_3 <- x %>% round_down_from(2, threshold = 3)

test_that("`round_up_from()` works correctly", {
  expect_equal(
    x_up,
    x_up %>% trunc_reverse() %>% round_up_from(2, threshold = 3)
  )
})

test_that("`round_down_from()` works correctly", {
  expect_equal(
    x_down,
    x_down %>% trunc_reverse() %>% round_down_from(2, threshold = 3)
  )
})


# Threshold 8 -------------------------------------------------------------

x_up_from_8   <- x %>% round_up_from(2, threshold = 8)
x_down_from_8 <- x %>% round_down_from(2, threshold = 8)

test_that("`round_up_from()` works correctly", {
  expect_equal(
    x_up,
    x_up %>% trunc_reverse() %>% round_up_from(2, threshold = 8)
  )
})

test_that("`round_down_from()` works correctly", {
  expect_equal(
    x_down,
    x_down %>% trunc_reverse() %>% round_down_from(2, threshold = 8)
  )
})
