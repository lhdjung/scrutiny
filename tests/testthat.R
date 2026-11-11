library(testthat)
library(scrutiny)

# CRAN allows two cores. testthat takes the number of workers from
# `options(Ncpus)` first, then from `TESTTHAT_CPUS`, so this caps both:
options(Ncpus = 2L)

test_check("scrutiny")
