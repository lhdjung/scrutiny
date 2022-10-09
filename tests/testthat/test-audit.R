


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
data_debit_seq <- debit_map_seq(pigs3)

audit_seq_grim  <- audit_seq(data_grim_seq)
audit_seq_debit <- audit_seq(data_debit_seq)

audit_seq_grim <- pigs1 %>%
  grim_map_seq() %>%
  audit_seq() %>%
  unclass_scr()

data_incons <- pigs1 %>%
  grim_map() %>%
  dplyr::filter(!consistency) %>%
  unclass_scr()


test_that("", {
  audit_seq_grim %>% dim() %>% expect_equal(c(8, 12))
  audit_seq_grim[1:3] %>% expect_equal(data_incons[1:3])
  audit_seq_grim[[4]] %>% expect_equal(c(4, 6, 6, 7, 3, 6, 8, 6))
  audit_seq_grim[[5]] %>% expect_equal(c(2, 3, 3, 3, 3, 3, 4, 3))
  audit_seq_grim[[6]] %>% expect_equal(c(2, 3, 3, 4, 0, 3, 4, 3))
  audit_seq_grim[[7]] %>% expect_equal(c(2, 1, 1, 1, 1, 1, 1, 1))
})


