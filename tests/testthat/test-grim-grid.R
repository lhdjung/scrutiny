test_that("cached grids equal computed ones", {
  for (key in c("1_up_or_down", "2_even", "2_anti_trunc")) {
    parts <- strsplit(key, "_", fixed = TRUE)[[1]]
    digits <- as.integer(parts[1])
    rounding <- paste(parts[-1], collapse = "_")
    digits |> generate_grim_grid(rounding) |> expect_identical(GRIM_GRIDS[[key]])
  }
})

test_that("`ties_*` methods and whole-number noise find the cached grid", {
  1             |> grim_grid("ties_up") |> expect_identical(GRIM_GRIDS[["1_up"]])
  (2 + 1e-12)   |> grim_grid()          |> expect_identical(GRIM_GRIDS[["2_up_or_down"]])
})

test_that("uncached methods are computed with their `threshold`", {
  1 |> grim_grid("up_from", threshold = 5) |> expect_identical(GRIM_GRIDS[["1_up"]])
  1 |> grim_grid("up_from", threshold = 9) |> expect_identical(generate_grim_grid(1, "up_from", 9))
})

test_that("invalid input is an error", {
  0         |> grim_grid()                  |> expect_error("must be 1, 2, or 3")
  4         |> grim_grid()                  |> expect_error("must be 1, 2, or 3")
  NA        |> grim_grid()                  |> expect_error("must be 1, 2, or 3")
  1.5       |> grim_grid()                  |> expect_error("whole numbers")
  c(1, 2)   |> grim_grid()                  |> expect_error("length")
  1         |> grim_grid(rounding = "nope") |> expect_error()
})
