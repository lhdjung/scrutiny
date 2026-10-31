audit_pigs1 <- pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1) |>
  audit()

audit_pigs2 <- pigs2 |>
  grim_map(digits_x = 1, items_averaged = 1, percent = TRUE) |>
  suppressMessages() |>
  audit()

audit_pigs1_exp <- tibble::tibble(
  all_cases = 12L,
  testable_cases = 12L,
  incon_cases = 8L,
  emp_incon = 0.6666666666666666,
  # Rows 1 and 4 have `n = 32` and `24`. Their means fall on rounding ties at
  # some values, which `"up_or_down"` admits in both directions:
  exp_incon = 0.7175,
  ratio = 0.9291521486643437,
)

audit_pigs2_exp <- tibble::tibble(
  all_cases = 6L,
  testable_cases = 6L,
  incon_cases = 5L,
  emp_incon = 0.8333333333333334,
  exp_incon = 0.85,
  ratio = 0.9803921568627452,
)


test_that("`audit()` works correctly for `grim_map()` output", {
  audit_pigs1 |> expect_equal(audit_pigs1_exp)
  audit_pigs2 |> expect_equal(audit_pigs2_exp)
})
