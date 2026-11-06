# The explanations are built from the numbers that decide the tests, and
# `grimmer_explain()` and `debit_explain()` abort if their own reading of these
# numbers disagrees with the verdict. A sweep over many value sets is what puts
# that check to work.

test_that("explanations agree with the tests (sweep)", {
  # Every data type takes its turn, each one on every third row:
  data_types <- c("integer", "continuous", NA)
  grid <- tidyr::expand_grid(
    x = c(0.05, 0.36, 0.5, 1.7, 3, 5.19, 5.23),
    sd = c(0, 0.11, 0.21, 0.49, 1.3, 2.55),
    n = c(2, 9, 28, 101),
    digits = 2,
    rounding = c("up_or_down", "up", "down", "even", "ceiling", "trunc")
  )

  for (i in seq_len(nrow(grid))) {
    row <- grid[i, ]
    args <- list(
      x = row$x,
      n = row$n,
      digits_x = row$digits,
      rounding = row$rounding
    )
    args_sd <- c(args, list(sd = row$sd, digits_sd = row$digits))
    data_type <- data_types[(i - 1L) %% 3L + 1L]
    args_grim <- c(args, list(items_averaged = 1, data_type = data_type))
    args_grimmer <- c(args_sd, list(items_averaged = 1, data_type = data_type))

    args_grim    |> do.call(what = grim_explain)    |> getElement("consistency") |> expect_identical(do.call(grim, args_grim))
    args_grimmer |> do.call(what = grimmer_explain) |> getElement("consistency") |> expect_identical(do.call(grimmer, args_grimmer))
    if (row$x <= 1 && row$sd <= 1) {
      args_debit <- c(args_sd, list(data_type = data_type))
      args_debit |> do.call(what = debit_explain) |> getElement("consistency") |> expect_identical(do.call(debit, args_debit))
    }
  }
})

test_that("`binary_ones_shown()` finds what listing every `k` would", {
  pick <- function(ones, n, range) {
    spread <- ones * (n - ones)
    passed <- spread >= range[1L] & spread <= range[2L]
    below <- spread < range[1L]
    list(
      any(passed),
      ones[passed][which.min(abs(spread[passed] - mean(range)))],
      ones[which.max(spread)],
      ones[which.min(spread)],
      ones[below][which.max(spread[below])],
      ones[!below][which.min(spread[!below])]
    )
  }

  set.seed(1)
  for (i in 1:2000) {
    n <- sample(c(2:60, 1000), 1L)
    ones <- sort(sample(0:n, 2L))
    range <- sample(0:(n^2 / 4 + 3), 2L)
    all <- as.numeric(ones[1L]:ones[2L])
    as.numeric(ones[1L]) |> binary_ones_shown(ones[2L], n, list(min = range[1L], max = range[2L])) |> pick(n, range) |> expect_identical(pick(all, n, range))
  }
})

test_that("only DEBIT's exact formula is explained", {
  0.5 |> debit_explain(sd = 0.5, n = 20, digits_x = 1, digits_sd = 1, data_type = "integer", formula = "mean_n") |> expect_error("only cover")
})

test_that("DEBIT's example SD is not shown as a bound", {
  0.5 |> debit_explain(sd = 0.5,   n = 2e5, digits_x = 1, digits_sd = 3, data_type = "integer", rounding = "down") |> format() |> expect_no_match("= 0.4995, which is in", fixed = TRUE)
  0   |> debit_explain(sd = 0.147, n = 320, digits_x = 1, digits_sd = 3, data_type = "integer", rounding = "down") |> format() |> expect_match("= 0.146504, which is in", fixed = TRUE)
})

# With `n` above `10^digits_x`, 499 / 999 printed as 0.4995, the bound it falls
# short of, and 50 / 101 as 0.495, the bound that `"down"` excludes:
test_that("GRIM's example means are not shown as bounds", {
  0.500 |> grim_explain(n = 999, digits_x = 3, items_averaged = 1, data_type = "integer")                   |> format() |> expect_match("499 / 999 = 0.499499", fixed = TRUE)
  0.500 |> grim_explain(n = 999, digits_x = 3, items_averaged = 1, data_type = "integer")                   |> format() |> expect_match("500 / 999 = 0.500501", fixed = TRUE)
  0.50  |> grim_explain(n = 101, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "down") |> format() |> expect_match("50 / 101 = 0.49505, which rounds to 0.50", fixed = TRUE)
})

test_that("a value that would look like a whole number or a bound gets just enough places", {
  6.99999999  |> format_number()                            |> expect_identical("6.99999999")
  0.146504464 |> format_number(digits = 3, bounds = 0.1465) |> expect_identical("0.146504")
  0.1465      |> format_number(digits = 3, bounds = 0.1465) |> expect_identical("0.1465")
})

test_that("explanations print as expected", {
  expect_snapshot({
    grim_explain(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")
    grim_explain(x = 5.19, n = 32, digits_x = 2, items_averaged = 1, data_type = "integer")
    grim_explain(x = 71, n = 43, digits_x = 0, items_averaged = 1, data_type = "integer", x_meaning = "percentage")
    grim_explain(x = 2.84, n = 16, digits_x = 2, items_averaged = 2, data_type = "integer", rounding = "up")
    grim_explain(x = 5.2, n = 20.5, digits_x = 1, items_averaged = 1, data_type = "integer")
    grimmer_explain(x = 5.23, sd = 2.55, n = 35, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
    grimmer_explain(x = 5.19, sd = 2.55, n = 28, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
    grimmer_explain(x = 3.1, sd = 1.3, n = 200, digits_x = 1, digits_sd = 1, items_averaged = 1, data_type = "integer")
    debit_explain(x = 0.36, sd = 0.11, n = 20, digits_x = 2, digits_sd = 2, data_type = "integer")
    debit_explain(x = 0.35, sd = 0.49, n = 20, digits_x = 2, digits_sd = 2, data_type = "integer")
    debit_explain(x = 0.36, sd = 0.49, n = 1000, digits_x = 2, digits_sd = 2, data_type = "integer")
  })
})

test_that("explanations say what the data type does", {
  expect_snapshot({
    grim_explain(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "continuous")
    grim_explain(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = NA)
    grim_explain(x = 5.19, n = 32, digits_x = 2, items_averaged = 1, data_type = NA)
    grimmer_explain(x = 5.23, sd = 2.55, n = 35, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "continuous")
  })
})

test_that("explanations of continuous data follow the verdict", {
  single <- grimmer_explain(5, 1, 1, 0, 0, 1, "continuous", sd_meaning = "sd_pop")
  single$consistency |> expect_false()
  single$paragraphs  |> expect_match("a single value has an SD of 0")
  single <- grimmer_explain(5, 1, 1, 0, 0, 1, NA, sd_meaning = "sd_pop")
  single$consistency |> expect_false()
  single$paragraphs  |> utils::tail(1L) |> expect_match("inconsistent either way")
  negative <- grimmer_explain(5, -1, 20, 0, 0, 1, "continuous")
  negative$consistency |> expect_false()
  negative$paragraphs  |> expect_match("no data set has a negative SD")
  1 |> debit_explain(0.5, 1, 1, 1, "continuous", sd_meaning = "sd_pop") |> _$consistency |> expect_false()
  # The same for both data types, so the type doesn't come up:
  undecidable <- grim_explain(5.2, 0, 1, 1, NA)
  undecidable$consistency |> expect_equal(NA)
  undecidable$paragraphs  |> expect_equal("The test needs n to be a positive integer, but it is 0.")
})

test_that("`data_type` must be a single value", {
  5.2 |> grim_explain(20, 1, 1, c("integer", "integer")) |> expect_error("must have length 1")
  5.2 |> grim_explain(20, 1, 1, NULL)                    |> expect_error("must have length 1")
})

test_that("a missing `digits_x` gets the usual error", {
  5.19 |> grim_explain(n = 28, items_averaged = 1, data_type = "integer") |> expect_error("digits_x")
})

test_that("missing `items_averaged` is undecidable, not an error", {
  5.19 |> grim_explain(n = 28, digits_x = 2, items_averaged = NA, data_type = "integer")                             |> getElement("consistency") |> expect_identical(NA)
  5.19 |> grimmer_explain(sd = 1.2, n = 28, digits_x = 2, digits_sd = 2, items_averaged = NA, data_type = "integer") |> getElement("consistency") |> expect_identical(NA)
})

test_that("large numbers are written out in full", {
  5.19 |> grim_explain(n = 1e5, digits_x = 2, items_averaged = 1, data_type = "integer")       |> format() |> expect_match("100000 integers", fixed = TRUE)
  0.5  |> debit_explain(sd = 0.5, n = 2e5, digits_x = 1, digits_sd = 1, data_type = "integer") |> format() |> expect_no_match("e+0", fixed = TRUE)
})

test_that("bounds of values with many decimal places are shown in full", {
  5.1234 |> grim_explain(n = 28, digits_x = 4, items_averaged = 1, data_type = "integer")                                |> format() |> expect_match("from 5.12335 to 5.12345", fixed = TRUE)
  5.2345 |> grimmer_explain(sd = 1.2345, n = 30, digits_x = 4, digits_sd = 4, items_averaged = 1, data_type = "integer") |> format() |> expect_match("from 5.23445 to 5.23455", fixed = TRUE)
  0.3621 |> debit_explain(sd = 0.4811, n = 500, digits_x = 4, digits_sd = 4, data_type = "integer")                      |> format() |> expect_match("from 0.36205 to 0.36215", fixed = TRUE)
})

test_that("`grim_explain()` shows its bounds exactly", {
  # The bound 5.18255 was shown as 5.1826, and the range of 2e14 as a point:
  5.19 |> grim_explain(28, 2, 1, rounding = "up_from", threshold = 2.55, data_type = "integer") |> format() |> paste(collapse = " ") |> expect_match("5.18255 * 28 = 145.1114", fixed = TRUE)
  2e14 |> grim_explain(3, 0, 1, data_type = "integer") |> format() |> paste(collapse = " ") |> expect_match("199999999999999.5 * 3 = 599999999999998.5", fixed = TRUE)
  -5.19 |> grim_explain(28, 2, 1, data_type = "integer") |> format() |> paste(collapse = " ") |> expect_match("-5.195 * 28 = -145.46", fixed = TRUE)
})


test_that("only a single case is explained", {
  c(5.19, 5.2) |> grim_explain(n = 28, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("length 1")
})


# Mappers -----------------------------------------------------------------

# `explain_map()` re-tests each row and aborts if the verdict differs from the
# row's `consistency` value, so explaining whole mapper outputs checks them all.

test_that("`explain_map()` agrees with every mapper tier", {
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")                                    |> explain_map() |> getElement("explanation") |> expect_length(12L)
  pigs1 |> grim_map(digits_x = 2, items_averaged = 2, data_type = "integer")                                    |> explain_map() |> getElement("explanation") |> expect_length(12L)
  pigs2 |> grim_map(digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = "percentage")                    |> explain_map() |> getElement("explanation") |> expect_length(6L)
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")                  |> explain_map() |> getElement("explanation") |> expect_length(12L)
  pigs3 |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer")                                        |> explain_map() |> getElement("explanation") |> expect_length(7L)
  pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")                                |> explain_map() |> getElement("explanation") |> expect_length(160L)
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", rounding = "up") |> explain_map() |> getElement("explanation") |> expect_length(12L)
})

test_that("`explain_map()` keeps the data as it is", {
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  explained <- data |> explain_map()
  explained |> class()            |> expect_identical(class(data))
  explained |> scrutiny_meta()    |> expect_identical(scrutiny_meta(data))
  explained |> dplyr::select(-explanation) |> expect_identical(data)
  data |> dplyr::filter(n > 1000) |> explain_map() |> getElement("explanation") |> expect_identical(character())
})

test_that("`explain_map()` uses the settings of the mapper call", {
  tibble::tibble(x = 71, n = 43) |> grim_map(digits_x = 0, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> explain_map() |> getElement("explanation") |> expect_match("x = 71, n = 43, items_averaged = 1, data_type = \"integer\", x_meaning = \"percentage\"", fixed = TRUE)
})

test_that("`explain_map()` catches rows changed after testing", {
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  data$n[1] <- 28
  data |> explain_map() |> expect_error("row with x = 7.22, n = 28, items_averaged = 1, data_type = \"integer\" doesn't match", fixed = TRUE)
})

test_that("`explain_map()` needs columns and settings", {
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> dplyr::select(-digits_x) |> explain_map() |> expect_error("Missing: `digits_x`")
  pigs3 |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer", formula = "mean_n")             |> explain_map() |> expect_error("was tested with")
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  attr(data, "scrutiny") <- NULL
  data |> explain_map() |> expect_error("lost the settings")
})

test_that("`explain_print()` explains only the rows it prints", {
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  data |> explain_print(2)                    |> expect_output("^GRIM test: x = 4.74, n = 25, items_averaged = 1, data_type = \"integer\"\n")
  data |> explain_print(1:2)                  |> expect_output("x = 4.74")
  data |> explain_print(-1)                   |> expect_output("x = 4.74")
  data |> dplyr::filter(!consistency) |> explain_print() |> expect_output("x = 4.74")

  # Row 2 would fail the re-test, but it is not printed:
  data$n[2] <- 50
  data |> explain_print(1)                    |> expect_output("x = 7.22")
  data |> explain_print(2)                    |> expect_error("row with x = 4.74, n = 50, items_averaged = 1, data_type = \"integer\" doesn't match", fixed = TRUE)

  capture.output(returned <- data |> explain_print(1))
  returned |> expect_identical(data)
})

test_that("`explain_map()` and `explain_print()` only take mapper output", {
  pigs1                                       |> explain_map()   |> expect_error("must be the output")
  pigs1                                       |> explain_print() |> expect_error("must be the output")
  pigs1 |> structure(class = c("scrutiny_add_up_to_map", class(pigs1))) |> explain_print() |> expect_error("must be the output")
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> dplyr::summarise(m = mean(x)) |> explain_print() |> expect_error("must be the output")
  1:3                                         |> explain_print() |> expect_error("must be a data frame")
})

test_that("`explain_map()` can be extended to other classes", {
  explain_map.my_map <- function(data) "my method"
  pigs1 |> structure(class = c("my_map", class(pigs1))) |> explain_map() |> expect_identical("my method")
})

test_that("`explain_print()` prints as expected", {
  expect_snapshot({
    pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> explain_map() |> explain_print(1:2)
    pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> dplyr::filter(n > 1000) |> explain_print()
  })
})


test_that("GRIMMER explains a sum of squares below what whole numbers can reach", {
  3.6 |> grimmer_explain(0.2, 11, digits_x = 1, digits_sd = 1, items_averaged = 1, data_type = "integer") |> _$paragraphs |> paste(collapse = " ") |> expect_match("can have is 148, with 7 of them 4 and 4 of them 3. So none of", fixed = TRUE)
  1.9 |> grimmer_explain(0.3, 25, digits_x = 1, digits_sd = 1, items_averaged = 1, data_type = "integer") |> _$paragraphs |> paste(collapse = " ") |> expect_match("can have is 91, with 22 of them 2 and 3 of them 1. It is in this range", fixed = TRUE)
})


test_that("GRIMMER explains a sum of squares that fewer than five numbers cannot have", {
  1 |> grimmer_explain(2, 2, digits_x = 1, digits_sd = 1, items_averaged = 1, data_type = "integer") |> _$paragraphs |> paste(collapse = " ") |> expect_match("and 2 integers with a sum of 2 cannot have 6.", fixed = TRUE)
  1 |> grimmer_explain(6, 4, digits_x = 1, digits_sd = 0, items_averaged = 1, data_type = "integer") |> _$paragraphs |> paste(collapse = " ") |> expect_match("integers with a sum of 4 can have is 98.", fixed = TRUE)
})


# Under `"even"`, a sum on a bound is a tie that `base::round()` may break
# either way, so the text used to say "1 / 4 = 0.25, which rounds to 0.2" and
# then "Consistent":
test_that("`grim_explain()` does not claim a rounding for a tie under `\"even\"`", {
  tie_only <- grim_explain(0.2, 4, digits_x = 1, items_averaged = 1, data_type = "integer", rounding = "even")
  tie_only$paragraphs[[3]] |> expect_match("a tie that rounding to even may break either way")
  # With another sum in the range, that one is shown instead:
  tie_and_more <- grim_explain(0.3, 20, digits_x = 1, items_averaged = 1, data_type = "integer", rounding = "even")
  tie_and_more$paragraphs[[3]] |> expect_match("6 / 20 = 0.3, which rounds to 0.3")
  grim_explain(0.2, 4, digits_x = 1, items_averaged = 1, data_type = "integer")$paragraphs[[3]] |> expect_match("which rounds to 0.3 or 0.2")
})

test_that("an inconsistent case names both roundings of a tie under `\"even\"`", {
  # 0.025 used to be said to round to 0.03, from the double's binary value:
  0.04  |> grim_explain(40, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "even")      |> _$paragraphs[[3]] |> expect_match("round to 0.02 or 0.03 (a tie) and 0.05", fixed = TRUE)
  2.986 |> grim_explain(80, digits_x = 3, items_averaged = 1, data_type = "integer", rounding = "ties_even") |> _$paragraphs[[3]] |> expect_match("2.987 or 2.988 (a tie)", fixed = TRUE)
  0.04  |> grim_explain(40, digits_x = 2, items_averaged = 1, data_type = "integer")                         |> _$paragraphs[[3]] |> expect_no_match("a tie")
  0.04  |> grimmer_explain(0.2, 40, 2, 2, 1, rounding = "even", data_type = "integer") |> format() |> paste(collapse = " ") |> expect_match("0.02 or 0.03 (a tie)", fixed = TRUE)
})

test_that("`grimmer_explain()` squares a negative sum as a whole", {
  out <- grimmer_explain(-3.15, 1.15, 20, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  out$paragraphs[[6]] |> expect_match("(-63)^2", fixed = TRUE)
})


test_that("a single score averaged from items is not \"each of the 1 scores\"", {
  5.5 |> grim_explain(1, digits_x = 1, items_averaged = 2, data_type = "integer") |> format() |> paste(collapse = " ") |> expect_match("The single score")
})


test_that("a near-whole `n` is explained as the whole number, as the tests test it", {
  1.5 |> grim_explain(2 - 1e-9, digits_x = 1, items_averaged = 1, data_type = "integer", rounding = "ceiling") |> format() |> expect_identical(format(grim_explain(1.5, 2, digits_x = 1, items_averaged = 1, data_type = "integer", rounding = "ceiling")))
  1.5 |> grimmer_explain(0.71, 2 - 1e-9, digits_x = 1, digits_sd = 2, items_averaged = 1, data_type = "integer") |> format() |> expect_identical(format(grimmer_explain(1.5, 0.71, 2, digits_x = 1, digits_sd = 2, items_averaged = 1, data_type = "integer")))
  0.5 |> debit_explain(0.58, 2 - 1e-9, digits_x = 1, digits_sd = 2, data_type = "integer")                       |> format() |> expect_identical(format(debit_explain(0.5, 0.58, 2, digits_x = 1, digits_sd = 2, data_type = "integer")))

  # The decimal count was written as 0.999999999:
  1.5 |> grim_explain(20, digits_x = 1 - 1e-9, items_averaged = 1, data_type = "integer")                         |> format() |> expect_identical(format(grim_explain(1.5, 20, digits_x = 1, items_averaged = 1, data_type = "integer")))
  1.5 |> grimmer_explain(0.71, 2, digits_x = 1, digits_sd = 2 - 1e-9, items_averaged = 1, data_type = "integer")   |> format() |> expect_identical(format(grimmer_explain(1.5, 0.71, 2, digits_x = 1, digits_sd = 2, items_averaged = 1, data_type = "integer")))
  0.5 |> debit_explain(0.58, 2, digits_x = 1 - 1e-9, digits_sd = 2, data_type = "integer")                       |> format() |> expect_identical(format(debit_explain(0.5, 0.58, 2, digits_x = 1, digits_sd = 2, data_type = "integer")))
})


test_that("a negative zero is shown as zero", {
  # `formatC()` printed it as "-0.0":
  -0 |> grim_explain(3, digits_x = 1, items_averaged = 1, data_type = "integer")    |> _$inputs |> expect_equal("x = 0.0, n = 3, items_averaged = 1, data_type = \"integer\"")
  -0 |> grimmer_explain(-0, 3, 1, 2, items_averaged = 1, data_type = "integer")     |> _$inputs |> expect_equal("x = 0.0, sd = 0.00, n = 3, items_averaged = 1, data_type = \"integer\"")
  -0 |> grimmer_explain(-0, 3, 1, 2, items_averaged = 1, data_type = "integer")     |> format() |> paste(collapse = " ") |> expect_no_match("-0.0[^0-9]")
})

test_that("an explanation names the missing value that left the mean unknown", {
  grimmer_explain(5.2, 0.3, 4, NA, 1, 1, data_type = "integer") |> format() |> expect_match("digits_x is missing")
  debit_explain(0.5, 0.9, 10, NA, 1, data_type = "integer")     |> format() |> expect_match("digits_x is missing")
  grimmer_explain(NA, 0.3, 1, 2, 1, 1, sd_meaning = "sd_pop", data_type = "integer") |> format() |> expect_match("1 integer with")
})
test_that("only the first `n` sums are tested, and the rest are counted", {
  # This looped over 20 million sums; all fail, as the first two do:
  text <- grimmer_explain(5, 1e-8, 2, digits_x = 0, digits_sd = 8, items_averaged = 1e7, data_type = "integer") |> format()
  text |> expect_match("The remaining 19999999 sums fail as well")
  text |> expect_match("Inconsistent")
})

test_that("sums out of reach of the SD are said to be skipped, and counted", {
  # The sums from 4500000 on were skipped, yet counted among "the next 499772
  # sums" after the three shown, from 4999549:
  text <- grimmer_explain(5, 0.01, 1e6, 0, 2, 1, data_type = "integer")$paragraphs |> paste(collapse = " ")
  text |> expect_match("more than 451 away from a multiple of 1000000")
  text |> expect_match("The other 499772 sums below 4999775 fail as well")
  text |> expect_no_match("next")
  # Nothing is skipped when every sum is within reach:
  grimmer_explain(3, 1.2, 6, 0, 1, 1, data_type = "integer")$paragraphs |> paste(collapse = " ") |> expect_no_match("Not every sum")
})

test_that("numbers past 2^53 are shown exactly", {
  # Doubles printed 27000720006300016, and named k = 149999999 as the largest:
  0.5 |> debit_explain(0.0009, 300003, 1, 4, sd_meaning = "se", data_type = "integer") |> format() |> expect_match("27000720006300018")
  e <- 0 |> debit_explain(0.4, 3e8 + 1, 0, 1, sd_meaning = "se", data_type = "integer") |> format()
  e |> expect_match("27000000180000000300000000")
  e |> expect_match("largest of these SEs, for k = 150000000")
})

test_that("`debit_explain()` writes the bounds of the number of ones in full", {
  # The product was rounded to 15 digits, 450000000000000, next to a range of
  # whole numbers ending at 449999999999999:
  0 |> debit_explain(0.3, 899999999999999, 0, 1, data_type = "integer") |> format() |> paste(collapse = " ") |> expect_match("0.5 * 899999999999999 = 449999999999999.5", fixed = TRUE)
  # The bound itself was rounded to 0.0222:
  0.03 |> debit_explain(0.17, 33, 2, 2, rounding = "up_from", threshold = 2.25, data_type = "integer") |> format() |> paste(collapse = " ") |> expect_match("0.02225 * 33 = 0.73425", fixed = TRUE)
})

test_that("`debit_explain()` explains a mean that fails with `digits_sd` missing", {
  # The SD's bounds were formatted with `NA` decimal places and failed:
  0.6 |> debit_explain(0.2, 2, 1, NA, data_type = "integer") |> format() |> paste(collapse = " ") |> expect_match("no number of ones fits")
  tibble::tibble(x = c(0.6, 0.5), sd = c(0.2, 0.71), n = 2) |> debit_map(digits_x = 1, digits_sd = c(NA, 2), data_type = "integer") |> explain_map() |> nrow() |> expect_identical(2L)
})

test_that("`debit_explain()` rejects a string `n`, as `debit()` does", {
  0.5 |> debit_explain(0.5, "10", 1, 1, data_type = "integer") |> expect_error("must be one of these types")
})

test_that("`explain_map()` doesn't overwrite a column of the user's", {
  out <- tibble::tibble(x = 5.19, n = 28, explanation = "mine") |> grim_map(2, items_averaged = 1, data_type = "integer")
  out |> explain_map() |> expect_error("already has an `explanation` column")
  # Its own column from an earlier call is the same, so it is kept:
  explained <- pigs1 |> grim_map(2, items_averaged = 1, data_type = "integer") |> explain_map()
  explained |> explain_map() |> expect_identical(explained)
})


test_that("explanations say which rounding they assume, once", {
  ROUNDING_METHODS |> purrr::map_chr(\(r) describe_rounding(r, 5, FALSE)) |> expect_length(length(ROUNDING_METHODS))
  5.19 |> grim_explain(28, 2, 1, "integer", rounding = "up_from", threshold = 4, symmetric = TRUE) |> _$paragraphs |> expect_match("(assuming rounding up from 4, with negative values rounded like their absolute values)", fixed = TRUE, all = FALSE)
  5.19 |> grim_explain(28, 2, 1, "integer", rounding = "ceiling", symmetric = TRUE)               |> _$paragraphs |> expect_match("(assuming always rounding up)", fixed = TRUE, all = FALSE)
  5.23 |> grimmer_explain(2.55, 31, 2, 2, 1, "integer") |> format() |> gregexpr(pattern = "assuming") |> _[[1L]] |> expect_length(1L)
})

test_that("explanations of percentages say so", {
  71 |> grim_explain(43, 0, 1, "continuous", x_meaning = "percentage") |> format() |> expect_match("can have any percentage")
  71 |> grim_explain(43, 0, 1, "integer", x_meaning = "percentage")    |> format() |> expect_match("not to 71%.", fixed = TRUE)
})


test_that("continuous data are explained by their own reason, not by a missing `n`", {
  # A missing `n` or `items_averaged` can be anything for continuous data, but
  # the text was the one for integers, where it leaves the case open: "n is
  # missing, and the case can't be decided without it" over "Inconsistent".
  text <- function(explanation) format(explanation) |> paste(collapse = " ")

  grimmer_explain(x = 5, sd = -1, n = NA, digits_x = 0, digits_sd = 0, items_averaged = 1, data_type = "continuous")  |> text() |> expect_match("no data set has a negative SD.*Inconsistent")
  grimmer_explain(x = 5, sd = -1, n = 10, digits_x = 0, digits_sd = 0, items_averaged = NA, data_type = "continuous") |> text() |> expect_match("no data set has a negative SD.*Inconsistent")
  grim_explain(x = Inf, n = NA, digits_x = 2, items_averaged = 1, data_type = "continuous")                           |> text() |> expect_match("x is infinite.*Undecidable")
  grim_explain(x = 2, n = NA, digits_x = 0, items_averaged = 0, data_type = "continuous")                             |> text() |> expect_match("number of items.*Undecidable")

  # The header still shows the values as given:
  grimmer_explain(x = 5, sd = -1, n = NA, digits_x = 0, digits_sd = 0, items_averaged = 1, data_type = "continuous") |> text() |> expect_match("n = NA")

  # Through the mapper:
  tibble::tibble(x = 5, sd = -1, n = NA_real_) |>
    grimmer_map(digits_x = 0, digits_sd = 0, items_averaged = 1, data_type = "continuous") |>
    explain_map() |>
    dplyr::pull(explanation) |>
    expect_match("no data set has a negative SD")
})


test_that("a large sum of squares with a fraction is not shown as an integer", {
  # 15 significant digits left none for the fraction from about 1e14 on, so
  # the range read "...420 to ...421", followed by "The only integer in this
  # range is ...421":
  text <- grimmer_explain(x = 12345678.3, sd = 2.5, n = 3, digits_x = 1, digits_sd = 1, items_averaged = 1, data_type = "integer") |>
    format() |>
    paste(collapse = " ") |>
    gsub(pattern = "\\s+", replacement = " ")
  text |> expect_match("= 457247320530420\\.[0-9] to")
  text |> expect_match("= 457247320530421\\.[0-9]\\. The only integer in this range is 457247320530421")

  format_number(457247320530420.34) |> expect_equal("457247320530420.3")
  format_number(457247320530420)    |> expect_equal("457247320530420")
  format_number(5.185)              |> expect_equal("5.185")
  format_number(145.18)             |> expect_equal("145.18")
})
