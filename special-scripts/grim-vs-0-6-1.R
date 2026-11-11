# Compares GRIM's verdicts with those of scrutiny 0.6.1, the last version before
# 1.0.0, over a grid of value sets. Under the settings below, the two must agree
# everywhere. 1.0.0 changed GRIM's verdicts only for the rounding methods with
# one-sided bounds ("ceiling", "floor", "trunc", "anti_trunc"), for non-default
# `threshold` and `symmetric`, and for cases that cannot be tested, which now
# return `NA`. Each version runs in its own R process, since both cannot be
# loaded at once. Takes a few minutes. Run from the package root:
#
#   Rscript special-scripts/grim-vs-0-6-1.R

grid <- rbind(
  expand.grid(i = 0:99,                n = 1:120,                digits = 1L, offset = c(0, 5, 47)),
  expand.grid(i = 0:99,                n = 1:250,                digits = 2L, offset = c(0, 3, 58)),
  expand.grid(i = seq(0, 999, by = 7), n = seq(1, 1200, by = 3), digits = 3L, offset = c(0, 2))
) # fmt: skip
grid$x <- round(grid$offset + grid$i / 10^grid$digits, grid$digits)

configs <- list(
  defaults = list(),
  items_2  = list(items = 2),
  percent  = list(percent = TRUE),
  up       = list(rounding = "up"),
  down     = list(rounding = "down"),
  even     = list(rounding = "even")
) # fmt: skip

if (length(commandArgs(TRUE)) == 0) {
  # Parent: install 0.6.1, get its verdicts from a child, compare.
  source("special-scripts/install-0-6-1.R")
  old_lib <- tempfile("scrutiny_0.6.1_")
  dir.create(old_lib)
  install_scrutiny_0_6_1(old_lib)
  old_file <- tempfile(fileext = ".rds")
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("special-scripts/grim-vs-0-6-1.R", old_file, old_lib)
  )
  stopifnot(status == 0)
  old <- readRDS(old_file)

  pkgload::load_all(".", quiet = TRUE)
  new <- configs |>
    lapply(function(config) {
      unname(do.call(
        grim,
        c(list(x = grid$x, n = grid$n, digits_x = grid$digits), config)
      ))
    })

  testthat::expect_equal(new, old)
  cat("All", length(configs) * nrow(grid), "verdicts agree.\n")
} else {
  # Child: 0.6.1 takes `x` as a string that carries its decimal places.
  child_args <- commandArgs(TRUE)
  library(scrutiny, lib.loc = child_args[2])
  x <- sprintf("%.*f", grid$digits, grid$x)
  old <- configs |>
    lapply(function(config) {
      unname(do.call(grim, c(list(x = x, n = grid$n), config)))
    })
  saveRDS(old, child_args[1])
}
