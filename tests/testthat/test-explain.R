# The explanations are built from the numbers that decide the tests, and
# `grimmer_explain()` and `debit_explain()` abort if their own reading of these
# numbers disagrees with the verdict. A sweep over many value sets is what puts
# that check to work.

test_that("explanations agree with the tests (sweep)", {
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
    args_grim <- c(args, list(items_averaged = 1))
    args_grimmer <- c(args_sd, list(items_averaged = 1))

    args_grim    |> do.call(what = grim_explain)    |> getElement("consistency") |> expect_identical(do.call(grim, args_grim))
    args_grimmer |> do.call(what = grimmer_explain) |> getElement("consistency") |> expect_identical(do.call(grimmer, args_grimmer))
    if (row$x <= 1 && row$sd <= 1) {
      args_sd    |> do.call(what = debit_explain)   |> getElement("consistency") |> expect_identical(do.call(debit, args_sd))
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
    as.numeric(ones[1L]) |> binary_ones_shown(ones[2L], n, range) |> pick(n, range) |> expect_identical(pick(all, n, range))
  }
})

test_that("only DEBIT's exact formula is explained", {
  0.5 |> debit_explain(sd = 0.5, n = 20, digits_x = 1, digits_sd = 1, formula = "mean_n") |> expect_error("only cover")
})

test_that("DEBIT's example SD is not shown as a bound", {
  0.5 |> debit_explain(sd = 0.5,   n = 2e5, digits_x = 1, digits_sd = 3, rounding = "down") |> format() |> expect_no_match("= 0.4995, which is in", fixed = TRUE)
  0   |> debit_explain(sd = 0.147, n = 320, digits_x = 1, digits_sd = 3, rounding = "down") |> format() |> expect_match("= 0.146504, which is in", fixed = TRUE)
})

test_that("a value that would look like a whole number or a bound gets just enough places", {
  6.99999999  |> format_number()                            |> expect_identical("6.99999999")
  0.146504464 |> format_number(digits = 3, bounds = 0.1465) |> expect_identical("0.146504")
  0.1465      |> format_number(digits = 3, bounds = 0.1465) |> expect_identical("0.1465")
})

test_that("explanations print as expected", {
  expect_snapshot({
    grim_explain(x = 5.19, n = 28, digits_x = 2, items_averaged = 1)
    grim_explain(x = 5.19, n = 32, digits_x = 2, items_averaged = 1)
    grim_explain(x = 71, n = 43, digits_x = 0, items_averaged = 1, percent = TRUE)
    grim_explain(x = 2.84, n = 16, digits_x = 2, items_averaged = 2, rounding = "up")
    grim_explain(x = 5.2, n = 20.5, digits_x = 1, items_averaged = 1)
    grimmer_explain(x = 5.23, sd = 2.55, n = 35, digits_x = 2, digits_sd = 2, items_averaged = 1)
    grimmer_explain(x = 5.19, sd = 2.55, n = 28, digits_x = 2, digits_sd = 2, items_averaged = 1)
    grimmer_explain(x = 3.1, sd = 1.3, n = 200, digits_x = 1, digits_sd = 1, items_averaged = 1)
    debit_explain(x = 0.36, sd = 0.11, n = 20, digits_x = 2, digits_sd = 2)
    debit_explain(x = 0.35, sd = 0.49, n = 20, digits_x = 2, digits_sd = 2)
    debit_explain(x = 0.36, sd = 0.49, n = 1000, digits_x = 2, digits_sd = 2)
  })
})

test_that("a missing `digits_x` gets the usual error", {
  5.19 |> grim_explain(n = 28, items_averaged = 1) |> expect_error("digits_x")
})

test_that("missing `items_averaged` is undecidable, not an error", {
  5.19 |> grim_explain(n = 28, digits_x = 2, items_averaged = NA)                             |> getElement("consistency") |> expect_identical(NA)
  5.19 |> grimmer_explain(sd = 1.2, n = 28, digits_x = 2, digits_sd = 2, items_averaged = NA) |> getElement("consistency") |> expect_identical(NA)
})

test_that("large numbers are written out in full", {
  5.19 |> grim_explain(n = 1e5, digits_x = 2, items_averaged = 1)       |> format() |> expect_match("100000 whole numbers", fixed = TRUE)
  0.5  |> debit_explain(sd = 0.5, n = 2e5, digits_x = 1, digits_sd = 1) |> format() |> expect_no_match("e+0", fixed = TRUE)
})

test_that("bounds of values with many decimal places are shown in full", {
  5.1234 |> grim_explain(n = 28, digits_x = 4, items_averaged = 1)                                |> format() |> expect_match("from 5.12335 to 5.12345", fixed = TRUE)
  5.2345 |> grimmer_explain(sd = 1.2345, n = 30, digits_x = 4, digits_sd = 4, items_averaged = 1) |> format() |> expect_match("from 5.23445 to 5.23455", fixed = TRUE)
  0.3621 |> debit_explain(sd = 0.4811, n = 500, digits_x = 4, digits_sd = 4)                      |> format() |> expect_match("from 0.36205 to 0.36215", fixed = TRUE)
})


test_that("only a single case is explained", {
  c(5.19, 5.2) |> grim_explain(n = 28, digits_x = 2, items_averaged = 1) |> expect_error("length 1")
})


# Mappers -----------------------------------------------------------------

# `explain_map()` re-tests each row and aborts if the verdict differs from the
# row's `consistency` value, so explaining whole mapper outputs checks them all.

test_that("`explain_map()` agrees with every mapper tier", {
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1)                                    |> explain_map() |> getElement("explanation") |> expect_length(12L)
  pigs1 |> grim_map(digits_x = 2, items_averaged = 2)                                    |> explain_map() |> getElement("explanation") |> expect_length(12L)
  pigs2 |> grim_map(digits_x = 1, items_averaged = 1, percent = TRUE)                    |> explain_map() |> getElement("explanation") |> expect_length(6L)
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1)                  |> explain_map() |> getElement("explanation") |> expect_length(12L)
  pigs3 |> debit_map(digits_x = 2, digits_sd = 2)                                        |> explain_map() |> getElement("explanation") |> expect_length(7L)
  pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1)                                |> explain_map() |> getElement("explanation") |> expect_length(160L)
  pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, rounding = "up") |> explain_map() |> getElement("explanation") |> expect_length(12L)
})

test_that("`explain_map()` keeps the data as it is", {
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1)
  explained <- data |> explain_map()
  explained |> class()            |> expect_identical(class(data))
  explained |> scrutiny_meta()    |> expect_identical(scrutiny_meta(data))
  explained |> dplyr::select(-explanation) |> expect_identical(data)
  data |> dplyr::filter(n > 1000) |> explain_map() |> getElement("explanation") |> expect_identical(character())
})

test_that("`explain_map()` uses the settings of the mapper call", {
  tibble::tibble(x = 71, n = 43) |> grim_map(digits_x = 0, items_averaged = 1, percent = TRUE) |> explain_map() |> getElement("explanation") |> expect_match("x = 71, n = 43, percent = TRUE", fixed = TRUE)
})

test_that("`explain_map()` catches rows changed after testing", {
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1)
  data$n[1] <- 28
  data |> explain_map() |> expect_error("row with x = 7.22, n = 28 doesn't match")
})

test_that("`explain_map()` needs columns and settings", {
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1) |> dplyr::select(-digits_x) |> explain_map() |> expect_error("Missing: `digits_x`")
  pigs3 |> debit_map(digits_x = 2, digits_sd = 2, formula = "mean_n")             |> explain_map() |> expect_error("was tested with")
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1)
  attr(data, "scrutiny") <- NULL
  data |> explain_map() |> expect_error("lost the settings")
})

test_that("`explain_print()` explains only the rows it prints", {
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1)
  data |> explain_print(2)                    |> expect_output("^GRIM test: x = 4.74, n = 25\n")
  data |> explain_print(1:2)                  |> expect_output("x = 4.74")
  data |> explain_print(-1)                   |> expect_output("x = 4.74")
  data |> dplyr::filter(!consistency) |> explain_print() |> expect_output("x = 4.74")

  # Row 2 would fail the re-test, but it is not printed:
  data$n[2] <- 50
  data |> explain_print(1)                    |> expect_output("x = 7.22")
  data |> explain_print(2)                    |> expect_error("row with x = 4.74, n = 50 doesn't match")

  capture.output(returned <- data |> explain_print(1))
  returned |> expect_identical(data)
})

test_that("`explain_map()` and `explain_print()` only take mapper output", {
  pigs1                                       |> explain_map()   |> expect_error("must be the output")
  pigs1                                       |> explain_print() |> expect_error("must be the output")
  pigs1 |> structure(class = c("scrutiny_add_up_to_map", class(pigs1))) |> explain_print() |> expect_error("must be the output")
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1) |> dplyr::summarise(m = mean(x)) |> explain_print() |> expect_error("must be the output")
  1:3                                         |> explain_print() |> expect_error("must be a data frame")
})

test_that("`explain_map()` can be extended to other classes", {
  explain_map.my_map <- function(data) "my method"
  pigs1 |> structure(class = c("my_map", class(pigs1))) |> explain_map() |> expect_identical("my method")
})

test_that("`explain_print()` prints as expected", {
  expect_snapshot({
    pigs1 |> grim_map(digits_x = 2, items_averaged = 1) |> explain_map() |> explain_print(1:2)
    pigs1 |> grim_map(digits_x = 2, items_averaged = 1) |> dplyr::filter(n > 1000) |> explain_print()
  })
})


test_that("GRIMMER explains a sum of squares below what whole numbers can reach", {
  3.6 |> grimmer_explain(0.2, 11, digits_x = 1, digits_sd = 1, items_averaged = 1) |> format() |> paste(collapse = " ") |> expect_match("can have is 148, with 7 of them 4 and 4 of them 3. So none of", fixed = TRUE)
  1.9 |> grimmer_explain(0.3, 25, digits_x = 1, digits_sd = 1, items_averaged = 1) |> format() |> paste(collapse = " ") |> expect_match("can have is 91, with 22 of them 2 and 3 of them 1. It is in this range", fixed = TRUE)
})


test_that("GRIMMER explains a sum of squares that fewer than five numbers cannot have", {
  1 |> grimmer_explain(2, 2, digits_x = 1, digits_sd = 1, items_averaged = 1) |> format() |> paste(collapse = " ") |> expect_match("and 2 whole numbers with a sum of 2 cannot have 6.", fixed = TRUE)
  1 |> grimmer_explain(6, 4, digits_x = 1, digits_sd = 0, items_averaged = 1) |> format() |> paste(collapse = " ") |> expect_match("numbers with a sum of 4 can have is 98.", fixed = TRUE)
})


# Under `"even"`, a sum on a bound is a tie that `base::round()` may break
# either way, so the text used to say "1 / 4 = 0.25, which rounds to 0.2" and
# then "Consistent":
test_that("`grim_explain()` does not claim a rounding for a tie under `\"even\"`", {
  tie_only <- grim_explain(0.2, 4, digits_x = 1, items_averaged = 1, rounding = "even")
  tie_only$paragraphs[[3]] |> expect_match("a tie that rounding to even may break either way")
  # With another sum in the range, that one is shown instead:
  tie_and_more <- grim_explain(0.3, 20, digits_x = 1, items_averaged = 1, rounding = "even")
  tie_and_more$paragraphs[[3]] |> expect_match("6 / 20 = 0.3, which rounds to 0.3")
  grim_explain(0.2, 4, digits_x = 1, items_averaged = 1)$paragraphs[[3]] |> expect_match("which rounds to 0.3 or 0.2")
})

test_that("`grimmer_explain()` squares a negative sum as a whole", {
  out <- grimmer_explain(-3.15, 1.15, 20, digits_x = 2, digits_sd = 2, items_averaged = 1)
  out$paragraphs[[6]] |> expect_match("(-63)^2", fixed = TRUE)
})


test_that("a single score averaged from items is not \"each of the 1 scores\"", {
  5.5 |> grim_explain(1, digits_x = 1, items_averaged = 2) |> format() |> paste(collapse = " ") |> expect_match("The single score")
})
