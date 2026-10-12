# Times `grim_map()` and `grimmer_map()` in scrutiny 0.6.1 (from CRAN) and in
# the working tree, on the same random data. Each version runs in its own R
# process, since both cannot be loaded at once. Run from the package root:
#
#   Rscript special-scripts/benchmark-vs-0-6-1.R
#
# Results, 2026-10-13, Apple M3, R 4.5.2 (median seconds of at least 5 runs):
#
#                 expression old_0_6_1 new_1_0_0 speedup
#      grim_map(), 1000 rows     0.167    0.0692     2.4
#     grim_map(), 10000 rows     1.660    0.6720     2.5
#    grimmer_map(), 300 rows     3.050    0.0392    77.8
#   grimmer_map(), 1000 rows    10.100    0.1220    82.6

if (length(commandArgs(TRUE)) == 0) {
  # Parent: install 0.6.1, run both children, compare.
  old_lib <- tempfile("scrutiny_0.6.1_")
  dir.create(old_lib)
  on.exit(unlink(old_lib, recursive = TRUE))
  install.packages(
    "https://cran.r-project.org/src/contrib/Archive/scrutiny/scrutiny_0.6.1.tar.gz",
    lib = old_lib,
    repos = NULL,
    type = "source",
    quiet = TRUE
  )
  if (!dir.exists(file.path(old_lib, "scrutiny"))) {
    install.packages(
      "https://cran.r-project.org/src/contrib/scrutiny_0.6.1.tar.gz",
      lib = old_lib,
      repos = NULL,
      type = "source",
      quiet = TRUE
    )
  }
  script <- "special-scripts/benchmark-vs-0-6-1.R"
  rscript <- file.path(R.home("bin"), "Rscript")
  old_file <- tempfile(fileext = ".rds")
  new_file <- tempfile(fileext = ".rds")
  system2(rscript, c(script, "old", old_file, old_lib))
  system2(rscript, c(script, "new", new_file))
  old <- readRDS(old_file)
  new <- readRDS(new_file)
  out <- data.frame(
    expression = old$expression,
    old_0_6_1 = signif(old$median, 3),
    new_1_0_0 = signif(new$median, 3),
    speedup = round(old$median / new$median, 1)
  )
  print(out, row.names = FALSE)
} else {
  # Child: time one version.
  args <- commandArgs(TRUE)
  old <- args[1] == "old"
  if (old) {
    library(scrutiny, lib.loc = args[3])
  } else {
    pkgload::load_all(".", quiet = TRUE)
  }

  set.seed(42)
  make_data <- function(rows) {
    x <- round(runif(rows, 1, 5), 2)
    sd <- round(runif(rows, 0.5, 2), 2)
    n <- sample(10:200, rows, replace = TRUE)
    if (old) {
      tibble::tibble(x = sprintf("%.2f", x), sd = sprintf("%.2f", sd), n = n)
    } else {
      tibble::tibble(x = x, sd = sd, n = n)
    }
  }
  data_300 <- make_data(300)
  data_1000 <- make_data(1000)
  data_10000 <- make_data(10000)

  # 0.6.1 warns about #80 on every GRIMMER call, so warnings are muffled in
  # both versions alike.
  run_grim <- function(data) {
    if (old) {
      grim_map(data[c("x", "n")])
    } else {
      grim_map(data[c("x", "n")], digits_x = 2)
    }
  }
  run_grimmer <- function(data) {
    suppressWarnings(
      if (old) {
        grimmer_map(data)
      } else {
        grimmer_map(data, digits_x = 2, digits_sd = 2)
      }
    )
  }

  result <- bench::mark(
    `grim_map(), 1000 rows` = run_grim(data_1000),
    `grim_map(), 10000 rows` = run_grim(data_10000),
    `grimmer_map(), 300 rows` = run_grimmer(data_300),
    `grimmer_map(), 1000 rows` = run_grimmer(data_1000),
    min_iterations = 5,
    check = FALSE,
    memory = FALSE,
    filter_gc = FALSE
  )
  saveRDS(
    list(
      expression = names(result$expression),
      median = as.numeric(result$median)
    ),
    args[2]
  )
}
