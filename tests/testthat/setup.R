# `grim_plot(split_by_digits = TRUE)` prints the plots it returns, and the tests
# print plots to check that drawing them raises no warnings. Running under
# `Rscript`/R CMD check, printing a plot with no device open implicitly opens
# the default `pdf()` device, writing an `Rplots.pdf` file into this directory.
# A null device absorbs that output instead:
grDevices::pdf(NULL)
