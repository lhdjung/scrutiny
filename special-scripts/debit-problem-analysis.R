
# NOTE: This script is meant for simulations that compare two versions of a
# certain if-else block in debit_table.R -- it is marked by comments there. To
# use this present file, run it, then outcomment the if-else block and incomment
# the one with the other version. It's not too elegant, but improving it would
# take more time.

number_samples <- 10000

random_x  <- number_samples %>% runif() %>% round(2) %>% restore_zeros(width = 2)
random_sd <- number_samples %>% runif() %>% round(2) %>% restore_zeros(width = 2)

out <- vector("list", number_samples)
out_n_incons <- integer(number_samples)

rounding_options <- c(
  "up_or_down",
  "up",
  "down",
  "ceiling",
  "floor",
  "trunc",
  "anti_trunc"
)


for (i in seq_len(number_samples)) {

  consistency <- logical(length(rounding_options))

  for (j in seq_along(rounding_options)) {
    x  <- random_x[i]
    sd <- random_sd[i]
    rounding <- rounding_options[j]

    # The outcommented parts here can be used to directly test the lower-level
    # `debit_table()` instead of the exported `debit()` function.
    consistency[j] <- debit(  # debit_table
      x = x,
      sd = sd,
      n = 468,
      # group_0 = NA,
      # group_1 = NA,
      formula = "mean_n",
      rounding = rounding,
      threshold = 5,
      symmetric = FALSE #,
      # show_rec = TRUE
    )
  }

  out[[i]] <- tibble::tibble(x, sd, rounding = rounding_options, consistency)

  if (i %% 500 == 0) {
    message(paste("Finished sample", i))
  }

}


# Count how many value sets per sample were consistent
for (i in seq_len(number_samples)) {
  out_n_incons[i] <- length(which(!(out[[i]][["consistency"]])))
}


# How many samples had one or more inconsistencies?
n_incons <- length(out_n_incons[out_n_incons != 0])
n_incons

# What is their proportion from among all samples?
n_incons / number_samples

# How many of these samples had both consistencies and inconsistencies (i.e., at
# least one rounding option was consistent, and at least one was inconsistent)?
n_incons_intermediate <-
  out_n_incons[out_n_incons != 0 & out_n_incons != length(rounding_options)] %>%
  length()

n_incons_intermediate

# What is their proportion from among all samples?
n_incons_intermediate / number_samples


