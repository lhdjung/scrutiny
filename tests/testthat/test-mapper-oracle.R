# Each mapper tier passes its settings down to the test, and a setting dropped on
# the way would raise no error: the test would quietly run under the default.
# Here, every row a mapper returns is tested again by `grim()`, `grimmer()`, or
# `debit()` under the settings the mapper was given, and the verdicts must
# agree. The data are rows of a grid on which leaving out any one setting
# changes some verdict, so the check cannot pass with a setting dropped.

# Each row's verdict, from the columns that are arguments of `fn`:
verdicts <- function(data, fn, settings) {
  args <- data[intersect(names(data), names(formals(fn)))]
  do.call(fn, c(as.list(args), settings))
}

# A matrix of rows by settings: does leaving out the setting change the row's
# verdict?
sensitive <- function(data, fn, settings) {
  full <- verdicts(data, fn, settings)
  vapply(
    names(settings),
    function(name) {
      without <- verdicts(data, fn, settings[names(settings) != name])
      is.na(without) != is.na(full) | (without != full) %in% TRUE
    },
    logical(nrow(data))
  )
}

# The first row of `grid` on which each setting matters:
pick_rows <- function(grid, fn, settings) {
  rows <- grid |> sensitive(fn, settings) |> apply(2L, \(x) which(x)[1L])
  rows |> anyNA() |> expect_false()
  grid[unique(rows), ]
}

expect_rows_agree <- function(out, fn, settings, label) {
  out$consistency |> expect_equal(verdicts(out, fn, settings), label = label)
  out |>
    sensitive(fn, settings) |>
    colSums() |>
    call_on(\(x) x > 0) |>
    all() |>
    expect_true(label = paste(label, "-- every setting matters"))
}

# All three tiers on one grid. The total-n mapper gets each picked value set
# twice, so that the split with no change in `n` is that value set:
expect_tiers_agree <- function(
  grid,
  mapper,
  mapper_seq,
  mapper_total_n,
  fn,
  args,
  settings
) {
  out <- do.call(mapper, c(list(grid), args, settings))
  out |> expect_rows_agree(fn, settings, "basic")

  # `args` go in as columns, so that `verdicts()` finds them:
  picked <- grid |>
    dplyr::mutate(!!!args) |>
    pick_rows(fn, settings) |>
    dplyr::select(dplyr::all_of(names(grid)))
  out <- do.call(
    mapper_seq,
    c(
      list(picked),
      args,
      settings,
      include_consistent = TRUE,
      include_reported = TRUE
    )
  )
  out |> expect_rows_agree(fn, settings, "seq")

  reported <- intersect(c("x", "sd"), names(picked))
  picked_twice <- picked[reported] |> dplyr::rename_with(\(x) paste0(x, 1L))
  picked_twice[paste0(reported, 2L)] <- picked[reported]
  picked_twice$n <- 2L * picked$n
  # Helper columns such as `items_averaged`:
  others <- setdiff(names(picked), c(reported, "n"))
  picked_twice[others] <- picked[others]
  out <- do.call(mapper_total_n, c(list(picked_twice), args, settings))
  out |> expect_rows_agree(fn, settings, "total-n")
}


test_that("every GRIM mapper tier agrees with `grim()` row by row", {
  grid <- tidyr::expand_grid(x = round(seq(-3, 3, by = 0.1), 1), n = c(2:30, 40L, 50L, 100L))
  settings <- list(rounding = "up_from", threshold = 3, symmetric = TRUE)
  grid |> expect_tiers_agree(grim_map, grim_map_seq, grim_map_total_n, grim, list(digits_x = 1, items_averaged = 2), settings)
})

test_that("every GRIM mapper tier agrees with `grim()` on percentages and an `items_averaged` column", {
  grid <- tidyr::expand_grid(x = seq(0, 100, by = 2.5), n = c(3:20, 40L, 50L), items_averaged = 1:3)
  settings <- list(percent = TRUE, rounding = "up_from", threshold = 3)
  grid |> expect_tiers_agree(grim_map, grim_map_seq, grim_map_total_n, grim, list(digits_x = 1), settings)
})

test_that("every GRIMMER mapper tier agrees with `grimmer()` row by row", {
  grid <- tidyr::expand_grid(x = round(seq(-2, 3, by = 0.1), 1), sd = c(0.3, 0.8, 1.1, 1.6), n = c(2:12, 20L, 50L))
  settings <- list(rounding = "up_from", threshold = 3, symmetric = TRUE, sd_meaning = "sd_pop")
  grid |> expect_tiers_agree(grimmer_map, grimmer_map_seq, grimmer_map_total_n, grimmer, list(digits_x = 1, digits_sd = 1, items_averaged = 2), settings)
})

test_that("every DEBIT mapper tier agrees with `debit()` row by row", {
  # `symmetric` is left out: DEBIT's values are never negative.
  grid <- tidyr::expand_grid(x = 0:100 / 100, sd = c(0.3, 0.46, 0.5), n = c(5L, 10L, 20L, 333L, 1000L))
  settings <- list(formula = "mean_n", rounding = "up_from", threshold = 3, sd_meaning = "sd_pop")
  grid |> expect_tiers_agree(debit_map, debit_map_seq, debit_map_total_n, debit, list(digits_x = 2, digits_sd = 2), settings)
})
