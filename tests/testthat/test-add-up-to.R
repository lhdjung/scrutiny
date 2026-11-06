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
  # With a single key column, the error used to end on "consistency with .":
  df |> dplyr::select(-total) |> add_up_to_map(summands = starts_with("n_")) |> expect_error("Column `total` must be in `data`") |> conditionMessage() |> expect_no_match("consistency with")
})

test_that("counts too large to add up exactly are undecidable", {
  c(1e20, 1) |> add_up_to(1e20)       |> expect_na()
  c(2^53, 1) |> add_up_to(2^53 + 1)   |> expect_na()
  c(2^52, 1) |> add_up_to(2^52 + 1)   |> expect_true()
})

test_that("near-whole values are added up as the whole numbers they are shown as", {
  c(10, 19)                 |> add_up_to(100 * 0.29) |> expect_true()
  c(10, 100 * 0.29)         |> add_up_to(39)         |> expect_true()
  c(100 * 0.57, 100 * 0.29) |> add_up_to(86)         |> expect_true()
  c(10, 100 * 0.29)         |> add_up_to(40)         |> expect_false()
})

test_that("a `NULL` element of `summands` is a type error", {
  add_up_to(list(NULL), 0)            |> expect_error("`summands` must be one of these types")
  add_up_to(list(1:2, NULL), c(3, 0)) |> expect_error("`summands` must be one of these types")
  add_up_to(list(character(0)), 0)    |> expect_error("`summands` must be one of these types")
  add_up_to(list(list()), 0)          |> expect_error("`summands` must be one of these types")
  add_up_to(list(numeric(0)), 0)      |> expect_true()
})

test_that("`add_up_to_map()` doesn't add up a factor's level codes", {
  # `c()` would combine the codes 1 and 2 with the numbers of `b`:
  f <- function(...) tibble::tibble(...) |> add_up_to_map(summands = c(a, b))
  f(a = factor(c(10, 20)),   b = c(3, 4),           total = c(13, 24)) |> expect_error("Can't combine `a` <factor")
  f(a = c(3, 4),             b = factor(c(10, 20)), total = c(13, 24)) |> expect_error("Can't combine `a` <double> and `b` <factor")
  f(a = Sys.Date() + 0:1,    b = c(3, 4),           total = c(13, 24)) |> expect_error("must have a common type")
  f(a = factor(c(10, 20)),   b = factor(c(3, 4)),   total = c(13, 24)) |> expect_error("It is a factor")
  f(a = c(10L, 20L),         b = c(3, 4),           total = c(13, 24)) |> _$consistency |> expect_equal(c(TRUE, TRUE))
})

test_that("missing `summands` get the package's own error", {
  add_up_to(total = 120) |> expect_error("Need to specify `summands`")
  add_up_to(c(1, 2))     |> expect_error("Need to specify `total`")
})
