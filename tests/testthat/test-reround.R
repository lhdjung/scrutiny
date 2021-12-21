

x <- rnorm(25000, 500, 30)

test_reround <- function(x, digits) {
  all(
    all(dplyr::near(reround(x, digits, "up"), round_up(x, digits))),
    all(dplyr::near(reround(x, digits, "down"), round_down(x, digits))),
    all(dplyr::near(reround(x, digits, "even"), round(x, digits))),
    all(dplyr::near(reround(x, digits, "ceiling"), round_ceiling(x, digits))),
    all(dplyr::near(reround(x, digits, "floor"), round_floor(x, digits))),
    all(dplyr::near(reround(x, digits, "trunc"), round_trunc(x, digits))),
    all(dplyr::near(reround(x, digits, "anti_trunc"), round_anti_trunc(x, digits)))
  )
}


test_that("`reround()` works like each of the specific rounding functions", {
  test_reround(x, 1:250) %>% expect_true()
})

