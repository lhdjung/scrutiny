# The three vectorized consistency test functions -- `grim()`, `grimmer()`, and
# `debit()` -- share one body, `map_value_sets()`. What is tested here is that
# shared behavior, on all three where it can differ between them.

# The arguments each of them vectorizes over: the key arguments, their
# `digits_*` arguments, and `items_averaged` where the test has it. This is the same
# split the mapper tier makes between what it takes from `data` and what it
# passes as a constant for the whole call.
args_along <- list(
  grim = c("x", "n", "digits_x", "items_averaged"),
  grimmer = c("x", "sd", "n", "digits_x", "digits_sd", "items_averaged"),
  debit = c("x", "sd", "n", "digits_x", "digits_sd")
)

# One call per test that is consistent, in a form the loops below can vary:
args_valid <- list(
  grim = list(x = 5.19, n = 32, digits_x = 2, items_averaged = 1),
  grimmer = list(
    x = 5.23, sd = 2.55, n = 31, digits_x = 2, digits_sd = 2, items_averaged = 1
  ),
  debit = list(x = 0.36, sd = 0.11, n = 20, digits_x = 2, digits_sd = 2)
)


test_that("the internal-only arguments are gone from the exported functions", {
  # `show_rec` and `show_reason` belong to the mapper tier: they are what makes
  # a `*_scalar()` function return its reconstructed values or its reason for an
  # inconsistency, so that `function_map()` can unpack them into columns. Both
  # were documented as "For internal use only" and yet were formals of the
  # exported functions, because `Vectorize()` copies every formal of the
  # function it wraps. A user who took the invitation got an unnamed matrix of
  # lists, one column per value set, rather than the logical vector these
  # functions promise.
  grim    |> formals() |> rlang::has_name("show_rec")    |> expect_false()
  grimmer |> formals() |> rlang::has_name("show_reason") |> expect_false()
  debit   |> formals() |> rlang::has_name("show_rec")    |> expect_false()

  5.19 |> grim(n = 28, digits_x = 2, items_averaged = 1, show_rec = TRUE) |> expect_error()
  0.36 |>
    debit(sd = 0.11, n = 20, digits_x = 2, digits_sd = 2, show_rec = TRUE) |>
    expect_error()
  5.23 |>
    grimmer(sd = 2.55, n = 31, digits_x = 2, digits_sd = 2, items_averaged = 1, show_reason = TRUE) |>
    expect_error()

  # `grim_map()` and `debit_map()` still have `show_rec`; `grimmer_map()` fixes
  # its `show_reason`:
  grim_map    |> formals() |> rlang::has_name("show_rec")    |> expect_true()
  grimmer_map |> formals() |> rlang::has_name("show_reason") |> expect_false()
  debit_map   |> formals() |> rlang::has_name("show_rec")    |> expect_true()
})


test_that("the return value is a logical vector, whatever the arguments", {
  for (name_fn in names(args_valid)) {
    fn <- get(name_fn)
    args <- args_valid[[name_fn]]

    out <- do.call(fn, args)
    out |> expect_type("logical")
    out |> expect_length(1L)
    out |> expect_named(NULL)

    # Several value sets at once:
    args_many <- args
    args_many$x <- rep(args$x, 3L)
    out_many <- do.call(fn, args_many)
    out_many |> expect_type("logical")
    out_many |> expect_length(3L)

    # No value set at all. `Vectorize()` returned an empty *list* here, because
    # `mapply()` has nothing to simplify:
    args_none <- args
    args_none$x <- numeric(0L)
    out_none <- do.call(fn, args_none)
    out_none |> expect_type("logical")
    out_none |> expect_length(0L)
  }
})


test_that("the key arguments are recycled against each other", {
  for (name_fn in names(args_valid)) {
    fn <- get(name_fn)
    args <- args_valid[[name_fn]]
    names_along <- args_along[[name_fn]]

    out_1 <- do.call(fn, args)

    # Each vectorized argument, given three times, produces the same verdict
    # three times; the others are still length 1 and are recycled to match:
    for (name_arg in names_along) {
      args_rep <- args
      args_rep[[name_arg]] <- rep(args_rep[[name_arg]], 3L)
      do.call(fn, args_rep) |> expect_equal(rep(out_1, 3L))
    }
  }
})


test_that("a length that cannot be recycled is an error", {
  # `mapply()` recycled anything into anything, and only warned when the longer
  # length was not a multiple of the shorter one. So `grim()` paired the third
  # `x` with the first `n` and reported a verdict for a value set that the
  # caller never wrote down:
  c(5.19, 5.18, 5.17) |>
    grim(n = c(28, 32), digits_x = 2, items_averaged = 1) |>
    expect_error("recycle")
  c(5.23, 5.23, 5.23) |>
    grimmer(sd = c(2.55, 2.55), n = 31, digits_x = 2, digits_sd = 2, items_averaged = 1) |>
    expect_error("recycle")
  c(0.36, 0.36, 0.36) |>
    debit(sd = c(0.11, 0.11), n = 20, digits_x = 2, digits_sd = 2) |>
    expect_error("recycle")

  # A multiple is no better than any other mismatch:
  c(5.19, 5.18, 5.17, 5.16) |>
    grim(n = c(28, 32), digits_x = 2, items_averaged = 1) |>
    expect_error("recycle")
})


test_that("arguments that describe the test as a whole must be length 1", {
  # These say *how* to test rather than *what* to test, so they cannot vary from
  # one value set to the next. `Vectorize()` looped over them along with the
  # values, so a call could ask for two rounding methods at once and get one
  # verdict per method rather than one per value set.
  args_scalar <- list(
    grim = list(
      rounding = c("up", "down"),
      threshold = c(5, 6),
      symmetric = c(TRUE, FALSE),
      percent = c(TRUE, FALSE)
    ),
    grimmer = list(
      rounding = c("up", "down"),
      threshold = c(5, 6),
      symmetric = c(TRUE, FALSE)
    ),
    debit = list(
      rounding = c("up", "down"),
      threshold = c(5, 6),
      symmetric = c(TRUE, FALSE),
      # Not the whole default set in its own order; see below:
      formula = c("mean_n", "exact")
    )
  )

  for (name_fn in names(args_scalar)) {
    fn <- get(name_fn)
    for (name_arg in names(args_scalar[[name_fn]])) {
      args <- args_valid[[name_fn]]
      args[[name_arg]] <- args_scalar[[name_fn]][[name_arg]]
      do.call(fn, args) |> expect_error("must be length 1")
    }
  }

  # Except an enum given as its whole default set, e.g. by a wrapper that
  # forwards its own default, which is its first value, as in the mappers:
  5.21 |> grimmer(1.21, 28, 2, 2, 1, sd_meaning = SD_MEANINGS) |> expect_equal(grimmer(5.21, 1.21, 28, 2, 2, 1))
  0.30 |> debit(0.46, 10, 2, 2, formula = DEBIT_FORMULAS)     |> expect_equal(debit(0.30, 0.46, 10, 2, 2))
  5.21 |> grimmer(1.21, 28, 2, 2, 1, sd_meaning = SD_MEANINGS[2:3]) |> expect_error("must be length 1")

  # The error says which arguments *are* vectorized, and points at the mapper:
  5.19 |>
    grim(n = 28, digits_x = 2, items_averaged = 1, rounding = c("up", "down")) |>
    expect_error("grim_map")
})


test_that("a missing `digits_*` argument still gets the bespoke error", {
  # The wrappers forward only the arguments the caller supplied, so that a
  # `digits_*` argument reaches the `*_scalar()` function as missing and
  # `error_digits_missing()` fires. Passing along a value that R invented would
  # produce a verdict for decimal places nobody stated.
  5.19 |> grim(n = 28, items_averaged = 1) |> suppressMessages() |> expect_error("digits_x")
  5.23 |>
    grimmer(sd = 2.55, n = 31, digits_x = 2, items_averaged = 1) |>
    suppressMessages() |>
    expect_error("digits_sd")
  0.36 |> debit(sd = 0.11, n = 20) |> suppressMessages() |> expect_error("digits_x")

  # ...and it is raised once, not once per value set:
  c(5.19, 5.18, 5.17) |>
    grim(n = 28, items_averaged = 1) |>
    suppressMessages() |>
    expect_error("digits_x")
})


test_that("the wrappers work however they are called", {
  # The supplied arguments are worked out with `missing()`, evaluated in the
  # wrapper's own frame. `match.call()` would be cheaper but cannot be trusted
  # here: a wrapper reached through `lapply()` is called as `FUN(X[[i]], ...)`,
  # and matching that call fails with "... used in a situation where it does not
  # exist".
  expected <- c(FALSE, TRUE)
  x <- c(5.19, 5.18)

  # The calls below are written out in full because their form is what is being
  # tested, so they don't start with `x |>`:
  grim(x = x, n = 28, digits_x = 2, items_averaged = 1)                     |> expect_equal(expected)
  grim(x, 28, 2, 1)                                                         |> expect_equal(expected)
  x |> grim(28, digits_x = 2, items_averaged = 1)                           |> expect_equal(expected)
  do.call(grim, list(x = x, n = 28, digits_x = 2, items_averaged = 1))      |> expect_equal(expected)
  lapply(x, grim, n = 28, digits_x = 2, items_averaged = 1) |> unlist()     |> expect_equal(expected)
  purrr::map_lgl(x, grim, n = 28, digits_x = 2, items_averaged = 1)         |> expect_equal(expected)
  Map(grim, x, 28, 2, 1) |> unlist()                                        |> expect_equal(expected)
  (function(...) grim(...))(x, 28, 2, 1)                                    |> expect_equal(expected)

  # Bound to a name of its own, and then that name bound to another test. The
  # `missing()` calls are cached per name, so the cache has to notice:
  fn <- grim
  fn(x, 28, 2, 1) |> expect_equal(expected)
  fn <- grimmer
  fn(5.23, 2.55, 31, 2, 2, 1) |> expect_true()
})


test_that("`grim()` and `grimmer()` take `items_averaged`", {
  # `items_averaged` is vectorized because the mapper tier lets it be a column of `data`.
  2.84 |> grim(n = 16, digits_x = 2, items_averaged = 2) |> expect_true()
  c(2.84, 2.84) |>
    grim(n = 16, digits_x = 2, items_averaged = c(1, 2)) |>
    expect_equal(c(
      grim(x = 2.84, n = 16, digits_x = 2, items_averaged = 1),
      grim(x = 2.84, n = 16, digits_x = 2, items_averaged = 2)
    ))
})
