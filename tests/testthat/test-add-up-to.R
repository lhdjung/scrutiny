test_that("`add_up_to()` compares the sum with the total", {
  c(40, 37, 43) |> add_up_to(120) |> expect_true()
  c(40, 37, 44) |> add_up_to(120) |> expect_false()
  list(c(40, 37, 43), c(10, 12)) |> add_up_to(c(120, 23)) |> expect_equal(c(TRUE, FALSE))
})

test_that("`add_up_to()` is `NA` for missing or fractional values", {
  c(1.5, 2)  |> add_up_to(3.5) |> expect_equal(NA)
  c(NA, 2)   |> add_up_to(3)   |> expect_equal(NA)
  c(1, 2)    |> add_up_to(NA)  |> expect_equal(NA)
  c("1", "2") |> add_up_to(3)  |> expect_error("summands")
})

test_that("`add_up_to_map()` adds up the selected columns", {
  df <- tibble::tibble(
    n_control = c(40, 25, 31),
    n_treatment = c(37, 26, 30),
    n_waitlist = c(43, 24, 30),
    total = c(120, 75, 90)
  )
  out <- df |> add_up_to_map(summands = starts_with("n_"))
  out$consistency |> expect_equal(c(TRUE, TRUE, FALSE))
  out |> colnames() |> expect_equal(c(colnames(df), "consistency"))
  out |> audit() |> getElement("incon_cases") |> expect_equal(1L)
  df |> add_up_to_map(summands = c(n_control, n_treatment)) |> getElement("consistency") |> expect_equal(c(FALSE, FALSE, FALSE))
  df |> add_up_to_map() |> expect_error("summands")
})

test_that("counts too large to add up exactly are undecidable", {
  c(1e20, 1) |> add_up_to(1e20)       |> expect_na()
  c(2^53, 1) |> add_up_to(2^53 + 1)   |> expect_na()
  c(2^52, 1) |> add_up_to(2^52 + 1)   |> expect_true()
})
