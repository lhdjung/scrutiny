# `grim_plot()` and `debit_plot()` always print the plot they return, as a
# design choice, not just when `split_by_digits = TRUE`. Running under
# `Rscript`/R CMD check, printing a plot with no device open implicitly opens
# the default `pdf()` device, writing an `Rplots.pdf` file into this directory.
# A null device absorbs that output instead:
grDevices::pdf(NULL)
