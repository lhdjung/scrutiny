

# `audit()` for GRIM
data_grim <- grim_map(pigs1)
audit_grim <- audit(data_grim)

test_that("`audit()` summarizes GRIM tests accurately", {
  expect_true(is.data.frame(audit_grim))
  expect_equal(as.numeric(audit_grim$all_cases - audit_grim$incons_cases),
               length(data_grim$consistency[data_grim$consistency]))
})



# `audit()` for DEBIT
data_debit <- debit_map(pigs3)
audit_debit <- audit(data_debit)

test_that("`audit()` summarizes DEBIT tests accurately", {
  expect_equal(as.numeric(audit_debit$incons_cases), 1)
  expect_true(dplyr::near(as.numeric(audit_debit$all_cases), 7))
  expect_true(dplyr::near(round(audit_debit$incons_rate, 3), 0.143))
})

