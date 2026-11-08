# `grimmer_values()` enumerates what `grimmer_scalar()` only searches: every
# whole-number sum that `sum_range()` admits for the mean and, for each, every
# sum of squares that `grimmer_candidate()` admits for the SD. The verdict and
# the argument checks come from `grimmer_scalar()` itself.

grimmer_values_scalar <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  # The same sum of squares gives a different SD under each premise, so the
  # values of the two can't be told apart in one table:
  sd_meaning <- check_sd_meaning_single(sd_meaning)
  # `"continuous"` is an error; a missing `data_type` is an error in
  # `grimmer_scalar()`, after the checks for the arguments before it:
  if (!missing(data_type)) {
    check_data_type_whole(data_type)
  }

  consistency <- grimmer_scalar(
    x = x,
    sd = sd,
    n = n,
    digits_x = digits_x,
    digits_sd = digits_sd,
    items_averaged = items_averaged,
    data_type = data_type,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    sd_meaning = sd_meaning
  )

  # As a double: `n * (n - 1)` overflows an integer from 46341:
  n_dev <- sd_denominator(as.numeric(n), sd_meaning)

  # One row of `NA` if undecidable, no rows if inconsistent. A consistent
  # value set with a missing value is forced to be consistent, but which
  # samples it stands for is still open:
  is_known <- !anyNA(list(
    x,
    sd,
    n,
    digits_x,
    digits_sd,
    items_averaged,
    data_type
  ))
  if (is.na(consistency) || (consistency && !is_known)) {
    return(grimmer_values_tibble(
      NA_real_,
      NA_real_,
      NA_real_,
      n,
      items_averaged,
      n_dev
    ))
  }
  if (!consistency) {
    return(grimmer_values_tibble(
      numeric(0L),
      numeric(0L),
      numeric(0L),
      n,
      items_averaged,
      n_dev
    ))
  }

  sd_bounds <- grimmer_sd_bounds(
    sd = sd,
    n = n,
    items_averaged = items_averaged,
    digits_sd = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    sd_meaning = sd_meaning
  )

  sums_consistent <- roundwork::sum_range(
    x_num = x,
    n_items = as.numeric(n) * items_averaged,
    digits = digits_x,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  value_set <- list(x = x, sd = sd, n = n, items_averaged = items_averaged)

  # A sum passes exactly if its remainder modulo `n` does, so the first `n`
  # sums tell which remainders pass, and only the sums with these are listed.
  # Only those within reach of the SD can, as in `grimmer_scalar()`. Each has
  # a pair at least, so they are counted before any is allocated: a mean with
  # many decimal places and a large `items_averaged` admits millions.
  abort_too_large <- abort_sum_squares_too_large
  # A sum of squares is at least the sum of squared deviations, so all of them
  # are too large if its lower bound is. Checked before the sums are tried:
  if (sd_bounds$term_lower$approx >= 2^53 * (1 + 2^-40)) {
    abort_too_large()
  }

  sum_last <- sums_consistent[[2L]]
  window <- sums_consistent |>
    grimmer_sums_reachable(n, sd_bounds) |>
    sums_from_bounds()
  passing <- window[vapply(
    window,
    function(s) {
      grimmer_candidate(
        s = s,
        n = n,
        items_averaged = items_averaged,
        sd_bounds = sd_bounds
      )$pass_test3
    },
    logical(1L)
  )]
  check_values_size(sum((sum_last - passing) %/% n + 1), "pairs", value_set)
  candidate_sums <- passing |>
    lapply(function(s) seq(s, sum_last, by = n)) |>
    unlist() |>
    sort()

  # One element per candidate sum, joined once at the end: appending to a
  # vector in the loop copies it each time, which gets quadratic by `n = 20000`.
  sums <- vector("list", length(candidate_sums))
  sums_squares <- sums
  deviations <- sums

  # Counted as they come, before anything big is allocated: the pairs grow with
  # `n^2` for SDs with few decimal places. At `x = 50`, `sd = 20`, `n = 5000`,
  # both with no decimals, there are 5e8.
  n_pairs <- 0

  for (i in seq_along(candidate_sums)) {
    s <- candidate_sums[i]
    candidate <- grimmer_candidate(
      s = s,
      n = n,
      items_averaged = items_averaged,
      sd_bounds = sd_bounds
    )

    if (!candidate$pass_test3) {
      next
    }

    # The test works with the sums of squares shifted to the sum `r`; the
    # output shows them as they are, which a double can only do below `2^53`:
    if (is.null(grimmer_candidate_absolute(candidate))) {
      abort_too_large()
    }

    r <- candidate$r
    upper <- candidate$range$upper

    squares <- if (n >= 5) {
      # Every second whole number from the first one with the parity of `s`:
      first <- candidate$lower + (candidate$lower - r) %% 2
      check_values_size(n_pairs + (upper - first) %/% 2 + 1, "pairs", value_set)
      seq(first, upper, by = 2)
    } else {
      # Fewer numbers skip some sums of squares, so they can't be counted
      # without finding them. `grimmer_candidate()` found the first one:
      sum_squares_attainable_all(
        s = r,
        n = n,
        first = candidate$attained,
        upper = upper,
        count_max = VALUES_MAX - n_pairs
      )
    }

    n_pairs <- n_pairs + length(squares)
    check_values_size(n_pairs, "pairs", value_set)

    sums[[i]] <- rep(s, length(squares))
    sums_squares[[i]] <- squares + candidate$shift
    # The sum of squared deviations, computed from the shifted sum of squares,
    # where it doesn't cancel against `s^2 / n` for a large mean:
    deviations[[i]] <- squares - r^2 / n
  }

  sums <- unlist(sums)
  sums_squares <- unlist(sums_squares)
  deviations <- unlist(deviations)

  if (length(sums) == 0L) {
    cli::cli_abort(
      "Internal error: `grimmer_values()` disagrees with `grimmer()`.",
      .internal = TRUE
    )
  }

  grimmer_values_tibble(
    sums,
    sums_squares,
    deviations,
    n,
    items_averaged,
    n_dev
  )
}


# The mean and SD that a sum and a sum of squared deviations of `n` integers give;
# see `sd_square_term()` for the formula. One row of `NA` but for `n` for an
# undecidable value set, so that it still has a row in the output.
grimmer_values_tibble <- function(
  sums,
  sums_squares,
  deviations,
  n,
  items_averaged,
  n_dev
) {
  tibble::tibble(
    # As doubles: the product of two integers overflows from 46,341 each:
    x = sums / (as.numeric(n) * items_averaged),
    sd = sqrt(deviations / n_dev) / items_averaged,
    n = as.numeric(n),
    sum = sums,
    sum_squares = sums_squares
  )
}

# `grimmer_values()` and `grimmer_closest()` show sums of squares as doubles,
# which skip whole numbers from `2^53` on:
abort_sum_squares_too_large <- function() {
  abort_in_export(
    "The sums of squares are too large to list.",
    "x" = "They reach 2^53, about 9e15, beyond which a double skips whole \\
    numbers.",
    "i" = "`grimmer()` tests this value set exactly all the same."
  )
}


# `grimmer_closest()`: of the pairs that whole numbers can have, the one whose
# SD is closest to `sd`. Like `grim_closest()`, it keeps a consistent value set
# to the pairs that would be reported as `x` and `sd`. Otherwise, it takes
# those whose mean would be reported as `x` or, if there are none, whose sum
# gives the mean that `grim_closest()` returns. Sums with the same remainder
# modulo `n` have the same SDs (see `grimmer_candidate()`), so each remainder
# is tried once, with its sum closest to `x`. Ties go to the mean closer to
# `x`, then to the smaller sum, then to the smaller SD.
grimmer_closest_scalar <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  sd_meaning <- check_sd_meaning_single(sd_meaning)
  if (!missing(data_type)) {
    check_data_type_whole(data_type)
  }

  consistency <- grimmer_scalar(
    x = x,
    sd = sd,
    n = n,
    digits_x = digits_x,
    digits_sd = digits_sd,
    items_averaged = items_averaged,
    data_type = data_type,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    sd_meaning = sd_meaning
  )

  n_dev <- sd_denominator(as.numeric(n), sd_meaning)

  # The pair depends on every value, so a missing one leaves it open even
  # where the verdict is forced:
  is_known <- !anyNA(list(
    x,
    sd,
    n,
    digits_x,
    digits_sd,
    items_averaged,
    data_type
  ))
  if (is.na(consistency) || !is_known) {
    return(grimmer_values_tibble(
      NA_real_,
      NA_real_,
      NA_real_,
      n,
      items_averaged,
      n_dev
    ))
  }

  n <- as.numeric(n)
  n_items <- n * items_averaged
  # The unrounded sum is `numerator / scale`. Distances from it are compared as
  # whole numbers, in units of the last decimal place of `x`, so that two means
  # equally far from it tie exactly, rather than by the noise in `x * n_items`:
  scale <- 10^digits_x
  numerator <- round(x * scale) * n_items
  sums <- roundwork::sum_range(
    x_num = x,
    n_items = n_items,
    digits = digits_x,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  if (consistency) {
    sd_bounds <- grimmer_sd_bounds(
      sd = sd,
      n = n,
      items_averaged = items_averaged,
      digits_sd = digits_sd,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric,
      sd_meaning = sd_meaning
    )
    if (sd_bounds$term_lower$approx >= 2^53 * (1 + 2^-40)) {
      abort_sum_squares_too_large()
    }
    remainders <- sums |>
      grimmer_sums_reachable(n, sd_bounds) |>
      sums_from_bounds()
    candidates <- remainders |>
      sums_closest_by_remainder(sums, n, numerator, scale)
    # Each is checked against the SD on its own, below:
    check_enumeration_size(
      c(1, length(candidates)),
      n,
      digits_x,
      limit = CLOSEST_SUMS_MAX,
      name = "`grimmer_closest()`"
    )
  } else if (sums[[1L]] > sums[[2L]]) {
    # No mean would be reported as `x`. Of the two sums around it, the one
    # whose mean `grim_closest()` returns:
    sd_bounds <- NULL
    candidates <- c(sums[[2L]], sums[[1L]])
    # A tie goes the same way as there, to the smaller one:
    distances <- abs(candidates * scale - numerator)
    candidates <- candidates[which.min(distances)]
  } else {
    sd_bounds <- NULL
    remainders <- if (sums[[2L]] - sums[[1L]] + 1 >= n) {
      seq_len(n) - 1
    } else {
      seq(sums[[1L]], sums[[2L]])
    }
    candidates <- remainders |>
      sums_closest_by_remainder(sums, n, numerator, scale)
  }

  r <- candidates %% n
  # The sum of squared deviations that `sd` stands for, plus `r^2 / n`, is the
  # shifted sum of squares to come closest to. A negative SD comes closest to
  # 0:
  deviations_target <- n_dev * (max(sd, 0) * items_averaged)^2
  targets <- deviations_target + r^2 / n
  # The candidates on either side of each target are found from the double,
  # which is fine as long as it is off by less than 1. From about 2^50.5, it
  # can be off by more, and the closer neighbor on the other side is missed:
  if (max(targets) >= 2^48) {
    abort_in_export(
      "The sums of squares are too large to find the closest one.",
      "x" = "They reach 2^48, about 3e14, beyond which `grimmer_closest()` \\
      can miss it.",
      "i" = "`grimmer()` tests this value set exactly all the same."
    )
  }
  # Distances between SDs are compared exactly, on whole numbers: `n` times the
  # sums of squared deviations, scaled by the square of `10^digits_sd`, which
  # makes the target whole as well. The roots themselves are too close to tell
  # apart from a sum of squares of about 2^14 on, so they only decide where
  # they are far apart; see `sd_closer()` and `roots_deviations()`:
  scale_sd <- 10^(2 * digits_sd)
  sd_whole <- round(max(sd, 0) * 10^digits_sd) * items_averaged
  target_whole <- whole_mul(whole_mul(n, n_dev), whole_mul(sd_whole, sd_whole))

  # The shifted sums of squares that the SD admits, if it is to be kept to
  # them, else all from the minimum `r` on:
  if (is.null(sd_bounds)) {
    lower <- r
    upper <- rep(Inf, length(r))
  } else {
    ranges <- vapply(
      candidates,
      function(s) {
        candidate <- grimmer_candidate(
          s = s,
          n = n,
          items_averaged = items_averaged,
          sd_bounds = sd_bounds
        )
        if (!candidate$pass_test3) {
          return(c(NA_real_, NA_real_))
        }
        if (is_big(candidate$range$upper)) {
          abort_sum_squares_too_large()
        }
        c(candidate$lower, candidate$range$upper)
      },
      numeric(2L)
    )
    # Remainders whose sums fail test 3 have no range:
    has_range <- !is.na(ranges[1L, ])
    candidates <- candidates[has_range]
    r <- r[has_range]
    targets <- targets[has_range]
    lower <- ranges[1L, has_range]
    upper <- ranges[2L, has_range]
  }

  squares <- if (n >= 5) {
    closest_square_parity(r, n, targets, lower, upper, target_whole, scale_sd)
  } else {
    vapply(
      seq_along(r),
      function(i) {
        closest_square_few(
          r[i],
          n,
          targets[i],
          lower[i],
          upper[i],
          target_whole,
          scale_sd
        )
      },
      numeric(1L)
    )
  }

  keep <- !is.na(squares)
  if (!any(keep)) {
    cli::cli_abort(
      "Internal error: `grimmer_closest()` disagrees with `grimmer()`.",
      .internal = TRUE
    )
  }
  candidates <- candidates[keep]
  r <- r[keep]
  squares <- squares[keep]

  # Not `squares - r^2 / n`: remainders `r` and `n - r` often have the same
  # deviations, and only this form keeps them equal, so that a tie goes by the
  # sum as documented:
  deviations <- (n * squares - r^2) / n
  distance_sum <- abs(candidates * scale - numerator)
  # The candidates whose SD is closest to `sd`. Doubles narrow them down to
  # those that may be (see `roots_deviations()`), which are compared exactly;
  # ties go by the sum:
  root_target <- sqrt(whole_double(target_whole))
  roots <- roots_deviations(squares, r, n, scale_sd)
  distance_sd <- abs(roots$root - root_target)
  near <- if (anyNA(roots$root)) {
    seq_along(roots$root)
  } else {
    tolerance <- tolerance_roots(
      max(roots$error),
      max(roots$error),
      root_target
    )
    which(distance_sd <= min(distance_sd) + tolerance)
  }
  tied <- near[sd_closest_all(
    deviations_whole(squares[near], r[near], n, scale_sd),
    target_whole
  )]
  best <- tied[order(distance_sum[tied], candidates[tied])[1L]]

  s <- candidates[best]
  shift <- (s %/% n) * (s + r[best])
  sum_squares <- squares[best] + shift
  if (abs(shift) >= 2^53 || abs(sum_squares) >= 2^53) {
    abort_sum_squares_too_large()
  }

  grimmer_values_tibble(
    s,
    sum_squares,
    deviations[best],
    n,
    items_averaged,
    n_dev
  )
}


# For each remainder modulo `n` among `remainders`, the sum with it that lies
# from `sums[1]` to `sums[2]` and is closest to the unrounded sum, which is
# `numerator / scale`. Compared as whole numbers, so that two sums equally
# close tie exactly, and the smaller one wins.
sums_closest_by_remainder <- function(remainders, sums, n, numerator, scale) {
  r <- unique(remainders %% n)
  target_floor <- numerator %/% scale
  below <- target_floor - ((target_floor - r) %% n)
  above <- below + n
  is_below <- numerator - below * scale <= above * scale - numerator
  closest <- ifelse(is_below, below, above)
  least <- sums[[1L]] + ((r - sums[[1L]]) %% n)
  greatest <- sums[[2L]] - ((sums[[2L]] - r) %% n)
  pmin(pmax(closest, least), greatest)
}


# Of the shifted sums of squares from `lower` to `upper` with the parity of
# `r`, the one closest to `target` in terms of the SD, for each remainder `r`
# at once; `NA` where there is none. From five whole numbers on, these are all
# the sums of squares that the numbers can have, from the minimum `r` on.
# `target_whole` and `scale_sd` are for the exact comparison; see
# `closest_by_sd()`.
closest_square_parity <- function(
  r,
  n,
  target,
  lower,
  upper,
  target_whole,
  scale_sd
) {
  below <- floor(pmin(target, upper))
  below <- below - (below - r) %% 2
  above <- ceiling(pmax(target, lower))
  above <- above + (above - r) %% 2
  below[below < lower] <- NA
  above[above > upper] <- NA
  closest_by_sd(below, above, r, n, target_whole, scale_sd)
}


# The same for fewer than five numbers, one remainder at a time, which can only
# have some of these; see `sum_squares_attainable()`. One number has only the
# minimum, which is 0 after the shift.
closest_square_few <- function(
  r,
  n,
  target,
  lower,
  upper,
  target_whole,
  scale_sd
) {
  if (n == 1) {
    return(if (lower <= 0 && 0 <= upper) 0 else NA_real_)
  }
  below <- square_attainable_below(r, n, lower, floor(min(target, upper)))
  above <- square_attainable_above(r, n, ceiling(max(target, lower)), upper)
  closest_by_sd(below, above, r, n, target_whole, scale_sd)
}


# Of two sums of squares around the target, either of them `NA`, the one whose
# SD is closer to `sd`, for each remainder `r` at once. The SD grows with the
# root of the sum of squared deviations, which is the shifted sum of squares
# minus `r^2 / n`. Compared exactly (see `sd_closer_each()`);
# equally close SDs go to the smaller one:
closest_by_sd <- function(below, above, r, n, target_whole, scale_sd) {
  is_below <- !is.na(below)
  both <- which(is_below & !is.na(above))
  is_below[both] <- sd_closer_each(
    below[both],
    above[both],
    r[both],
    n,
    target_whole,
    scale_sd
  ) <=
    0
  ifelse(is_below, below, above)
}


# `sd_closer()` for shifted sums of squares `below` and `above`, neither `NA`,
# with remainders `r`, all at once. The roots as doubles decide where their
# distances differ by far more than rounding error, the exact comparison the
# rest, which is much slower; see `roots_deviations()`:
sd_closer_each <- function(below, above, r, n, target_whole, scale_sd) {
  root_target <- sqrt(whole_double(target_whole))
  root_below <- roots_deviations(below, r, n, scale_sd)
  root_above <- roots_deviations(above, r, n, scale_sd)
  gap <- abs(root_below$root - root_target) - abs(root_above$root - root_target)
  tolerance <- tolerance_roots(root_below$error, root_above$error, root_target)
  out <- sign(gap)
  for (i in which(is.na(gap) | abs(gap) <= tolerance)) {
    out[i] <- sd_closer(
      deviations_whole(below[i], r[i], n, scale_sd)[[1L]],
      deviations_whole(above[i], r[i], n, scale_sd)[[1L]],
      target_whole
    )
  }
  out
}


# The roots of the whole numbers from `deviations_whole()`, as doubles, and
# bounds on their errors in units of `.Machine$double.eps`. Below 2^53,
# `n * squares - r^2` is exact, and the scaling and the root round once each.
# From 2^53 on, the product rounds as well, and the difference magnifies that
# error by `product / difference`. Where the difference isn't positive as a
# double, the root is `NA`, which leaves the comparison to the exact path:
roots_deviations <- function(squares, r, n, scale_sd) {
  product <- n * squares
  difference <- product - r^2
  is_exact <- product < 2^53
  magnified <- ifelse(is_exact, 1, product / difference)
  root <- sqrt(pmax(difference, 0) * scale_sd)
  root[!is_exact & difference <= 0] <- NA
  list(root = root, error = root * (magnified + 2))
}

# A bound on the error of a difference between the distances of two roots,
# with errors `error_1` and `error_2` from `roots_deviations()`, to the target's
# root, with four times the margin:
tolerance_roots <- function(error_1, error_2, root_target) {
  4 * .Machine$double.eps * (error_1 + error_2 + 2 * root_target)
}


# `n` times the sum of squared deviations behind each shifted sum of squares
# `squares` with the remainder `r`, scaled by `scale_sd`, so that it is a whole
# number on the scale of `target_whole`. A list, as each may be a `big()`
# number; `NA` stays `NA`:
deviations_whole <- function(squares, r, n, scale_sd) {
  lapply(seq_along(squares), function(i) {
    if (is.na(squares[i])) {
      return(NA_real_)
    }
    whole_mul(whole_diff(whole_mul(n, squares[i]), r[i]^2), scale_sd)
  })
}


# Which of two SDs is closer to a third, for whole numbers `below <= target <=
# above` that the squares of the three are proportional to: -1 if the SD at
# `below` is, 1 if the one at `above` is, and 0 if they are equally close. The
# one below is closer if `sqrt(target) - sqrt(below) < sqrt(above) -
# sqrt(target)`, i.e., `2 * sqrt(target) < sqrt(above) + sqrt(below)`. Squared,
# that is `4 * target - above - below < 2 * sqrt(above * below)`, which holds
# if the left side is negative, and otherwise if its square is below `4 *
# above * below`. All whole numbers, so the comparison is exact, where the
# roots themselves are not: next to a square `k^2`, the squares `k^2 - 1` and
# `k^2 + 1` have roots that differ in their distance to `k` by about
# `1 / (4 * k^3)`, which a double no longer shows from `k` of about 100 on:
sd_closer <- function(below, above, target) {
  sum_ab <- whole_sum(above, below)
  four_target <- whole_mul(4, target)
  if (whole_cmp(four_target, sum_ab) < 0) {
    return(-1)
  }
  left <- whole_diff(four_target, sum_ab)
  whole_cmp(whole_mul(left, left), whole_mul(whole_mul(4, above), below))
}


# The indices of the `deviations`, whole numbers as from `deviations_whole()`,
# whose SD is closest to the one at `target`: the greatest at or below it, the
# least at or above it, or both if they are equally close, and every one
# equal to these:
sd_closest_all <- function(deviations, target) {
  sides <- vapply(deviations, whole_cmp, numeric(1L), target)
  best <- function(side, pick) {
    index <- which(sides == side | sides == 0)
    if (length(index) == 0L) {
      return(NULL)
    }
    value <- deviations[[index[1L]]]
    for (i in index[-1L]) {
      if (pick(whole_cmp(deviations[[i]], value))) {
        value <- deviations[[i]]
      }
    }
    value
  }
  below <- best(-1, function(cmp) cmp > 0)
  above <- best(1, function(cmp) cmp < 0)
  closer <- if (is.null(below)) {
    1
  } else if (is.null(above)) {
    -1
  } else {
    sd_closer(below, above, target)
  }
  is_tied <- vapply(
    deviations,
    function(value) {
      (closer <= 0 && whole_cmp(value, below) == 0) ||
        (closer >= 0 && whole_cmp(value, above) == 0)
    },
    logical(1L)
  )
  which(is_tied)
}


# The least sum of squares from `lower` to `upper` that `n` from 2 to 4 whole
# numbers with the sum `r` can have, searched in windows that double in size,
# since `upper` may be infinite; or `NA`.
square_attainable_above <- function(r, n, lower, upper) {
  lower <- max(lower, r)
  width <- 64
  while (lower <= upper) {
    end <- min(lower + width, upper)
    check_grimmer_attainable_exact(end, n)
    found <- sum_squares_attainable(r, n, lower, end)
    if (!is.na(found)) {
      return(found)
    }
    lower <- end + 1
    width <- 2 * width
  }
  NA_real_
}


# The greatest sum of squares from `lower` to `upper`, as above:
square_attainable_below <- function(r, n, lower, upper) {
  lower <- max(lower, r)
  width <- 64
  while (upper >= lower) {
    start <- max(upper - width, lower)
    check_grimmer_attainable_exact(upper, n)
    first <- sum_squares_attainable(r, n, start, upper)
    if (!is.na(first)) {
      # A window of `width + 1` numbers has at most that many:
      all <- sum_squares_attainable_all(
        r,
        n,
        first,
        upper,
        count_max = width + 1
      )
      return(max(all))
    }
    upper <- start - 1
    width <- 2 * width
  }
  NA_real_
}


#' Achievable means and SDs behind a reported pair
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grimmer_values()` returns every pair of a mean and an SD that a sample of
#'   `n` integers can have and that would be reported as `x` and `sd`. It
#'   is the counterpart of [`grim_values()`] for [`grimmer()`], which is `TRUE`
#'   exactly if there is at least one such pair.
#'
#'   `grimmer_closest()` returns the single achievable pair whose SD is
#'   closest to `sd`, like [`grim_closest()`] for means. If `x` and `sd` are
#'   consistent, it is one of the pairs that `grimmer_values()` returns.
#'   Otherwise, it is closest among the pairs whose mean would be reported as
#'   `x`, or, if there are none, whose mean is the one `grim_closest()`
#'   returns.
#'
#'   Each pair comes with the whole-number sum and sum of squares behind it.
#'   Statistics computed from several groups, such as a t-value, can be
#'   compared exactly through these integers.
#'
#' @details The sums are the whole numbers that [`grim_values()`] divides by
#'   `n * items_averaged`. For each, the sums of squares are the whole numbers
#'   within the rounding bounds of `sd` that `n` integers with that sum can
#'   have. With `items_averaged > 1`, the integers are the respondents'
#'   totals across the items, so the sums are those of the totals.
#'
#'   The number of pairs grows with `n`, `items_averaged`, and `sd`, and
#'   shrinks with `digits_x` and `digits_sd`. More than a million pairs for one
#'   value set is an error, since the output has one row for each. So is a sum
#'   of squares of 2^53 or more, about 9e15, which a double can't hold exactly;
#'   [`grimmer()`] tests such value sets all the same. `grimmer_closest()`
#'   stops at 2^48, about 3e14, beyond which it can miss the closest pair.
#'   With `n` from 2 to 4, both functions stop at 2^40, about 1.1e12, as
#'   [`grimmer()`] does; `grimmer_closest()` even if `x` fails GRIM, since it
#'   still looks for the closest pair.
#'
#'   Under `rounding = "even"`, the pairs include ties that [`base::round()`]
#'   may round away from `x` or `sd`, as in [`grim_values()`].
#'
#'   In `grimmer_closest()`, pairs whose SDs are equally close go by the mean
#'   closer to `x`, then by the smaller sum, then by the smaller SD. A negative
#'   `sd` is closest to the least SD there is. The number of pairs doesn't
#'   matter there, but that of the sums does: for a consistent value set, it
#'   tries one sum per remainder modulo `n`, and more than 100,000 of them is
#'   an error. That takes an `n` of more than about `10^(digits_x + 5)`.
#'
#' @inheritParams grimmer
#' @param data_type String. `"integer"` if the values are whole numbers,
#'   as in [`grimmer()`]. There is no default, so that every call states
#'   this assumption. `"continuous"` is an error: such data can have any
#'   value that would be reported as the one given, so there are none to
#'   list. Under `NA`, a value set is undecidable unless no data can have it.
#' @param sd_meaning String. As in [`grimmer()`], but `"sd_sample_or_pop"` is
#'   not allowed: the same sum of squares gives a different SD under each
#'   premise, so call `grimmer_values()` once for each. Under `"se"`, the `sd`
#'   column holds the standard error.
#'
#' @return A tibble with one row per achievable pair. `case` is the index of
#'   the value set, as in [`grim_values()`]. `x` and `sd` are the achievable
#'   values, `n` the reported sample size, and `sum` and `sum_squares` the
#'   whole numbers behind them. An inconsistent value set has no rows, and one
#'   that cannot be decided has one row of `NA` but for `n`, as in
#'   [`grimmer()`].
#'
#'   `grimmer_closest()` returns the same columns with exactly one row per
#'   value set: of `NA` but for `n` if a value is missing or the value set
#'   cannot be decided.
#'
#' @seealso [`grimmer()`], [`grimmer_map()`], [`grim_values()`], and
#'   [`grimmer_plot_line()`].
#'
#' @include grimmer.R
#'
#' @export
#'
#' @examples
#' # A mean of 5.23 and an SD of 2.55 are consistent with a sample size of 31,
#' # and there is exactly one way to get there:
#' grimmer_values(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 31,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1,
#'   data_type = "integer"
#' )
#'
#' # With one decimal place, several pairs round to the same values:
#' grimmer_values(
#'   x = 5.3,
#'   sd = 0.7,
#'   n = 30,
#'   digits_x = 1,
#'   digits_sd = 1,
#'   items_averaged = 1,
#'   data_type = "integer"
#' )
#'
#' # None at all if the value set is inconsistent:
#' grimmer_values(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 35,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1,
#'   data_type = "integer"
#' )
#'
#' # ...but there is a pair close to it:
#' grimmer_closest(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 35,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1,
#'   data_type = "integer"
#' )

# The signature mirrors `grimmer_values_scalar()`'s; see `map_value_sets()`.
grimmer_values <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  map_value_sets(
    fn = grimmer_values_scalar,
    along = c(
      "x",
      "sd",
      "n",
      "digits_x",
      "digits_sd",
      "items_averaged",
      "data_type"
    ),
    type = "list"
  ) |>
    bind_cases(PTYPE_GRIMMER_VALUES)
}


#' @rdname grimmer_values
#' @export
grimmer_closest <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  map_value_sets(
    fn = grimmer_closest_scalar,
    along = c(
      "x",
      "sd",
      "n",
      "digits_x",
      "digits_sd",
      "items_averaged",
      "data_type"
    ),
    type = "list"
  ) |>
    bind_cases(PTYPE_GRIMMER_VALUES)
}
