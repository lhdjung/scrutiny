
# Alternatively to the below:
function() {
  df <- tibble::tibble(x = "0.637", n = 1:180) %>%
    grim_map(show_rec = TRUE)
}

grim_plot_best_guess <- function(x,
                                 n,
                                 items = 1,
                                 percent = FALSE,
                                 rounding = "up_or_down",
                                 threshold = 5,
                                 symmetric = FALSE,
                                 tolerance = .Machine$double.eps^0.5) {

  check_length(x, 1L)

  if (length(n) == 1L) {
    cli::cli_warn("This function is meant for a range of `n` values.")
  }

  consistency <- logical(length(n))
  rec_x_per_n <- numeric(length(n))

  for (i in seq_along(n)) {
    result <- grim_best_guess(
      x = x,
      n = n[i],
      items = items,
      percent = percent,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric,
      tolerance = tolerance
    )
    consistency[i] <- result[1]
    rec_x_per_n[i] <- result[2]
  }

  consistency <- as.logical(consistency)
  rec_x_per_n <- as.numeric(rec_x_per_n)

  df <- tibble::tibble(
    x = rec_x_per_n,
    n,
    consistency
  )

  n_intercepts_vertical <- df$x[df$consistency]
  x_num <- as.numeric(x)

  ggplot2::ggplot(df, ggplot2::aes(x = n, y = x)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = x_num, linetype = 2) +
    ggplot2::geom_vline(xintercept = n_intercepts_vertical)

}




grim_best_guess <- function(x,
                            n,
                            items = 1,
                            percent = FALSE,
                            rounding = "up_or_down",
                            threshold = 5,
                            symmetric = FALSE,
                            tolerance = .Machine$double.eps^0.5) {

  # Run the GRIM test, returning intermediary variables along with consistency,
  # the main result:
  grim_result <- grim_scalar(
    x = x,
    n = n,
    items = items,
    percent = percent,
    show_rec = TRUE,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    tolerance = tolerance
  )

  # Retrieve the reconstructed means -- the internal variables `rec_x_upper` and
  # `rec_x_lower` from `grim_scalar()` -- as a single numeric vector:
  rec_x_all <- unlist(
    x = c(
      grim_result[3],
      grim_result[4]
    ),
    use.names = FALSE
  )

  # Calculate the absolute difference between the reconstructed means and the
  # rounded reported mean:
  rounding_error_all <- abs(rec_x_all - as.numeric(x))

  # Find the one reconstructed mean that is closest to (i.e., minimally
  # different from) the reported mean:
  rec_x_all_closest <- rec_x_all[rounding_error_all == min(rounding_error_all)]

  # Return the consistency result and the closest value. In case there are
  # multiple closest values, only take the first one:
  list(
    grim_result[1],
    rec_x_all_closest[1]
  )

}


# # Can insert this in `grim_scalar()` right before the rethrns start:
# cat("\n")
# print(paste("n:", n))
# print(paste("rec_sum:", rec_sum))
# print(paste("rec_x_upper:", round(rec_x_upper[1], 2))) # first element only because of `dustify()`
# print(paste("rec_x_lower:", round(rec_x_lower[1], 2))) # first element only because of `dustify()`
# print(paste("grains_rounded[1]:", grains_rounded[1]))
# print(paste("grains_rounded[2]:", grains_rounded[2]))

