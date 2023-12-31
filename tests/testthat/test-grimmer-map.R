
# Example data ------------------------------------------------------------

pigs5_renamed <- pigs5 %>%
  dplyr::rename(q = x, w = sd, e = n)


# Expected output ---------------------------------------------------------

pigs5_exp <- tibble::tibble(
  x = c(
    "7.22", "4.74", "5.23", "2.57", "6.77", "2.68", "7.01", "7.38", "3.14",
    "6.89", "5.00", "0.24"
  ),
  sd = c(
    "5.30", "6.55", "2.55", "2.57", "2.18", "2.59", "6.68", "3.65", "5.32",
    "4.18", "2.18", "6.43"
  ),
  n = c(38, 31, 35, 30, 33, 34, 35, 32, 33, 37, 31, 34),
  consistency = c(
    FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE,
    TRUE, TRUE, TRUE
  ),
  reason = c(
    "GRIM inconsistent", "Passed all", "GRIMMER inconsistent (test 3)",
    "GRIMMER inconsistent (test 3)", "GRIM inconsistent", "Passed all",
    "GRIM inconsistent", "Passed all", "GRIM inconsistent", "Passed all",
    "Passed all", "Passed all"
  ),
) %>%
  structure(
    class = c("scr_grimmer_map", "scr_rounding_up_or_down", "tbl_df", "tbl", "data.frame")
  )


# Testing -----------------------------------------------------------------

test_that("`grimmer_map()` works correctly by default", {
  pigs5 %>% grimmer_map() %>% expect_equal(pigs5_exp)
})

test_that("`grimmer_map()` works correctly with columns renamed", {
  pigs5_renamed %>% grimmer_map(x = q, sd = w, n = e) %>% expect_equal(pigs5_exp)
})


