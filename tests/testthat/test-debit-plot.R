

plot <- debit_plot(debit_map(pigs3))

test_that("`debit_plot()` returns a ggplot object", {
  expect_true(ggplot2::is.ggplot(plot))
})

# test_that("The plot list has a structure with value `NULL`", {
#   expect_true(is.null(str(plot)$data))
# })
