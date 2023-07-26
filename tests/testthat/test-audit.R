


# `audit()` ---------------------------------------------------------------


# `audit()` for GRIM
data_grim  <- grim_map(pigs1)
audit_grim <- audit(data_grim)

test_that("`audit()` summarizes GRIM tests accurately", {
  expect_true(is.data.frame(audit_grim))
  expect_equal(as.numeric(audit_grim$all_cases - audit_grim$incons_cases),
               length(data_grim$consistency[data_grim$consistency]))
})



# `audit()` for DEBIT
data_debit  <- debit_map(pigs3)
audit_debit <- audit(data_debit)

test_that("`audit()` summarizes DEBIT tests accurately", {
  expect_equal(as.numeric(audit_debit$incons_cases), 1)
  expect_true(dplyr::near(as.numeric(audit_debit$all_cases), 7))
  expect_true(dplyr::near(round(audit_debit$incons_rate, 3), 0.143))
})



# `audit_seq()` -----------------------------------------------------------

data_grim_seq  <- grim_map_seq(pigs1)
data_grimmer_seq <- grimmer_map_seq(pigs5)
data_debit_seq <- debit_map_seq(pigs3)

# The scrutiny class is removed for the GRIM tibble because the latter is tested
# as an example for equality with tibbles that don't have that class:
audit_seq_grim    <- data_grim_seq    %>% audit_seq() %>% unclass_scr()
audit_seq_grimmer <- data_grimmer_seq %>% audit_seq()
audit_seq_debit   <- data_debit_seq   %>% audit_seq()

data_incons <- pigs1 %>%
  grim_map() %>%
  dplyr::filter(!consistency) %>%
  unclass_scr()


test_that("`audit_seq()` has correct output", {
  audit_seq_grim %>% dim() %>% expect_equal(c(8, 12))
  audit_seq_grim[1:3] %>% expect_equal(data_incons[1:3])
  audit_seq_grim[[4]] %>% expect_equal(c(4, 6, 6, 7, 3, 6, 8, 6))
  audit_seq_grim[[5]] %>% expect_equal(c(2, 3, 3, 3, 3, 3, 4, 3))
  audit_seq_grim[[6]] %>% expect_equal(c(2, 3, 3, 4, 0, 3, 4, 3))
  audit_seq_grim[[7]] %>% expect_equal(c(2, 1, 1, 1, 1, 1, 1, 1))
})


hits_total_is_correct <- function(audit_seq_output) {
  expected <- audit_seq_output %>%
    dplyr::rowwise() %>%
    dplyr::select(starts_with("hits"), -hits_total) %>%
    dplyr::mutate(hits_total_expected = sum(dplyr::c_across(everything()))) %>%
    dplyr::pull(hits_total_expected)

  all(expected == audit_seq_output$hits_total)
}


test_that("the `hits_total` column correctly sums up
          the other `hits_` columns", {
  audit_seq_grim    %>% hits_total_is_correct() %>% expect_true()
  audit_seq_grimmer %>% hits_total_is_correct() %>% expect_true()
  audit_seq_debit   %>% hits_total_is_correct() %>% expect_true()
})

