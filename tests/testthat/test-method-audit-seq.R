
# Expected output ---------------------------------------------------------

grim_exp <- tibble::tibble(
  x = c("4.74", "5.23"),
  n = c(25, 29),
  consistency = c(FALSE, FALSE),
  hits_total = c(4L, 6L),
  hits_x = 2:3,
  hits_n = 2:3,
  diff_x = c(2, 1),
  diff_x_up = c(2, 1),
  diff_x_down = c(-2, -2),
  diff_n = c(2, 1),
  diff_n_up = c(2, 1),
  diff_n_down = c(-2, -3),
) %>%
  structure(class = c("scr_audit_seq", "tbl_df", "tbl", "data.frame"))

grimmer_exp <- tibble::tibble(
  x = c("7.22", "5.23"),
  sd = c("5.30", "2.55"),
  n = c(38, 35),
  consistency = c(FALSE, FALSE),
  hits_total = c(8L, 16L),
  hits_x = c(4L, 2L),
  hits_sd = c(0L, 10L),
  hits_n = c(4L, 4L),
  diff_x = c(1, 3),
  diff_x_up = c(2, 3),
  diff_x_down = c(-1, -3),
  diff_sd = c(NA, 1),
  diff_sd_up = c(NA, 1),
  diff_sd_down = c(NA, -1),
  diff_n = c(1, 4),
  diff_n_up = c(2, 4),
  diff_n_down = c(-1, -4),
) %>%
  structure(class = c("scr_audit_seq", "tbl_df", "tbl", "data.frame"))

debit_exp <- tibble::tibble(
  x = "0.19",
  sd = "0.35",
  n = 1683L,
  consistency = FALSE,
  hits_total = 4L,
  hits_x = 2L,
  hits_sd = 2L,
  hits_n = 0L,
  diff_x = 4,
  diff_x_up = NA_real_,
  diff_x_down = -4,
  diff_sd = 4,
  diff_sd_up = 4,
  diff_sd_down = NA_real_,
  diff_n = NA_real_,
  diff_n_up = NA_real_,
  diff_n_down = NA_real_,
) %>%
  structure(class = c("scr_audit_seq", "tbl_df", "tbl", "data.frame"))


# Testing -----------------------------------------------------------------

test_that("`audit()` for `audit_seq()` works correctly", {
  pigs1[1:3, ] %>% grim_map_seq()    %>% audit_seq() %>% expect_equal(grim_exp)
  pigs5[1:3, ] %>% grimmer_map_seq() %>% audit_seq() %>% expect_equal(grimmer_exp)
  pigs3[4, ]   %>% debit_map_seq()   %>% audit_seq() %>% expect_equal(debit_exp)
})

