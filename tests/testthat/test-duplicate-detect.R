
# Expected output ---------------------------------------------------------

pigs4_exp <- tibble::tibble(
  snout = c("4.736", "8.131", "4.221", "4.221", "5.179"),
  snout_dup = c(FALSE, TRUE, TRUE, TRUE, FALSE),
  tail = c("6.887", "7.331", "6.095", "7.574", "8.131"),
  tail_dup = rep(c(FALSE, TRUE), c(4L, 1L)),
) %>%
  structure(class = c("scr_dup_detect", "tbl_df", "tbl", "data.frame"))

pigs4_missings <- pigs4
pigs4_missings[3, 1] <- NA
pigs4_missings[5, 2] <- NA

pigs4_missings_exp <- tibble::tibble(
  snout = c("4.736", "8.131", NA, "4.221", "5.179"),
  snout_dup = c(FALSE, FALSE, NA, FALSE, FALSE),
  tail = c("6.887", "7.331", "6.095", "7.574", NA),
  tail_dup = rep(c(FALSE, NA), c(4L, 1L)),
) %>%
  structure(class = c("scr_dup_detect", "tbl_df", "tbl", "data.frame"))


# Testing -----------------------------------------------------------------

test_that("`duplicate_detect()` works correctly by default", {
  pigs4 %>% duplicate_detect() %>% expect_equal(pigs4_exp)
})

test_that("`duplicate_detect()` works correctly with missings", {
  pigs4_missings %>% duplicate_detect() %>% expect_equal(pigs4_missings_exp)
})

