# Compares GRIM's verdicts with those of scrutiny 0.6.1, the last version before
# 1.0.0, over a grid of value sets. Under the settings below, the two must agree
# everywhere. 1.0.0 changed GRIM's verdicts only for the rounding methods with
# one-sided bounds ("ceiling", "floor", "trunc", "anti_trunc"), for non-default
# `threshold` and `symmetric`, and for cases that cannot be tested, which now
# return `NA`.
#
# The comparison downloads 0.6.1 from CRAN and runs it in a separate R process,
# which takes a few minutes, so it is off by default. Set this to `TRUE` to run:
compare_with_0_6_1 <- FALSE

skip_if_not(compare_with_0_6_1)

test_that("GRIM's verdicts agree with those of scrutiny 0.6.1", {
  grid <- rbind(
    expand.grid(i = 0:99,                n = 1:120,                digits = 1L, offset = c(0, 5, 47)),
    expand.grid(i = 0:99,                n = 1:250,                digits = 2L, offset = c(0, 3, 58)),
    expand.grid(i = seq(0, 999, by = 7), n = seq(1, 1200, by = 3), digits = 3L, offset = c(0, 2))
  )
  grid$x <- round(grid$offset + grid$i / 10^grid$digits, grid$digits)

  configs <- list(
    defaults = list(),
    items_2  = list(items = 2),
    percent  = list(percent = TRUE),
    up       = list(rounding = "up"),
    down     = list(rounding = "down"),
    even     = list(rounding = "even")
  )

  old_lib <- tempfile("scrutiny_0.6.1_")
  dir.create(old_lib)
  on.exit(unlink(old_lib, recursive = TRUE))
  tarball <- file.path(old_lib, "scrutiny_0.6.1.tar.gz")
  urls <- paste0(
    "https://cran.r-project.org/src/contrib/",
    c("Archive/scrutiny/", ""),
    "scrutiny_0.6.1.tar.gz"
  )

  for (url in urls) {
    downloaded <- tryCatch(
      download.file(url, tarball, quiet = TRUE) == 0,
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (downloaded) break
  }

  skip_if_not(downloaded, "Could not download scrutiny 0.6.1")
  install.packages(
    tarball,
    lib = old_lib,
    repos = NULL,
    type = "source",
    quiet = TRUE
  )

  # 0.6.1 takes `x` as a string that carries its decimal places
  input <- file.path(old_lib, "input.rds")
  output <- file.path(old_lib, "output.rds")
  script <- file.path(old_lib, "old.R")
  saveRDS(list(grid = grid, configs = configs), input)
  writeLines(
    c(
      sprintf("library(scrutiny, lib.loc = '%s')", old_lib),
      sprintf("input <- readRDS('%s')", input),
      "x <- sprintf('%.*f', input$grid$digits, input$grid$x)",
      "old <- lapply(input$configs, function(args) unname(do.call(grim, c(list(x = x, n = input$grid$n), args))))",
      sprintf("saveRDS(old, '%s')", output)
    ),
    script
  )

  status <- system2(file.path(R.home("bin"), "Rscript"), script)
  status |> expect_equal(0)
  old <- readRDS(output)

  new <- configs |>
    lapply(function(args) {
      unname(do.call(
        grim,
        c(list(x = grid$x, n = grid$n, digits_x = grid$digits), args)
      ))
    })

  new |> expect_equal(old)
})
