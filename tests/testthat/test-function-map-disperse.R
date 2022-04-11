

df1 <- tibble::tibble(
  y1 = 15:25,
  y2 = 25:35,
  n  = seq(from = 10, to = 30, by = 2)
)


# First, create a mock consistency test, called SCHLIM. It's analogous to GRIM
# as implemented in scrutiny, which is also true for the function names:
schlim_scalar <- function(x, n) {   # Note: `grim_scalar()` is not exported
  if (x > n) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Not needed, but included for completeness:
schlim <- Vectorize(schlim_scalar)

# This will be the input function:
schlim_map <- function(data) {
  consistency <- purrr::map2_lgl(
    as.numeric(data$x),
    as.numeric(data$n),
    schlim_scalar
  )
  return(dplyr::mutate(data, consistency))
}

# Use the function factory:
schlim_map_disperse <- function_map_disperse(
  .fun = schlim_map, .reported = c("y1", "y2"), .name_test = "SCHLIM"
)


# Apply the manufactured function to `df1`:
df1_tested <- schlim_map_disperse(df1, show_all = TRUE) %>%
  suppressMessages()


# Conduct tests:
test_that("The manufactured function has the correct length", {
  df1_tested %>% length() %>% expect_equal(3)
})

test_that("Its elements have the correct length", {
  df1_tested[[1]] %>% length() %>% expect_equal(8)
  df1_tested[[2]] %>% length() %>% expect_equal(11)
  df1_tested[[3]] %>% length() %>% expect_equal(11)
})




