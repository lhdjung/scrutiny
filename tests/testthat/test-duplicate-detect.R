
# Expected output ---------------------------------------------------------

pigs4_exp <- tibble::tibble(
  snout = c("4.73", "8.13", "4.22", "4.22", "5.17"),
  snout_dup = rep(c(FALSE, TRUE), c(1L, 4L)),
  tail = c("6.88", "7.33", "5.17", "7.57", "8.13"),
  tail_dup = c(FALSE, FALSE, TRUE, FALSE, TRUE),
  wings = c("6.09", "8.27", "4.40", "5.92", "5.17"),
  wings_dup = rep(c(FALSE, TRUE), c(4L, 1L)),
) %>%
  structure(class = c("scr_dup_detect", "tbl_df", "tbl", "data.frame"))

pigs4_missings <- pigs4
pigs4_missings[3, 1] <- NA
pigs4_missings[5, 2] <- NA

pigs4_missings_exp <- tibble::tibble(
  snout = c("4.73", "8.13", NA, "4.22", "5.17"),
  snout_dup = c(FALSE, FALSE, NA, FALSE, TRUE),
  tail = c("6.88", "7.33", "5.17", "7.57", NA),
  tail_dup = c(FALSE, FALSE, TRUE, FALSE, NA),
  wings = c("6.09", "8.27", "4.40", "5.92", "5.17"),
  wings_dup = rep(c(FALSE, TRUE), c(4L, 1L)),
) %>%
  structure(class = c("scr_dup_detect", "tbl_df", "tbl", "data.frame"))


# Testing -----------------------------------------------------------------

test_that("`duplicate_detect()` works correctly by default", {
  pigs4 %>% duplicate_detect() %>% expect_equal(pigs4_exp)
})

test_that("`duplicate_detect()` works correctly with missings", {
  pigs4_missings %>% duplicate_detect() %>% expect_equal(pigs4_missings_exp)
})

