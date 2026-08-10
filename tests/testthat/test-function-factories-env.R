# The bodies of factory-made functions call scrutiny-internal helpers such as
# `absorb_key_args()` and `check_factory_dots()` by their bare names. Those
# helpers can only be found if the manufactured function is enclosed in an
# environment that inherits from scrutiny's namespace. `function_map_total_n()`
# used to enclose it in the caller's environment instead, so any factory-made
# function exported from another package failed at the first such call. See
# issue #69.

# A mock consistency test, as in test-function-map-total-n.R:
schlim_scalar <- function(y, n) {
  (y / 3) > n
}

schlim_map <- function(data) {
  consistency <- purrr::map2_lgl(
    as.numeric(data$y),
    as.numeric(data$n),
    schlim_scalar
  )
  dplyr::mutate(data, consistency)
}

fns_factory_made <- list(
  map = function_map(
    .fun = schlim_scalar,
    .reported = c("y", "n"),
    .name_test = "SCHLIM"
  ),
  map_seq = function_map_seq(
    .fun = schlim_map,
    .reported = c("y", "n"),
    .name_test = "SCHLIM"
  ),
  map_total_n = function_map_total_n(
    .fun = schlim_map,
    .reported = "y",
    .name_test = "SCHLIM"
  )
)

# Is scrutiny's namespace among the ancestors of `fn`'s enclosing environment?
inherits_from_scrutiny_ns <- function(fn) {
  env <- environment(fn)
  while (!identical(env, emptyenv())) {
    if (identical(env, asNamespace("scrutiny"))) {
      return(TRUE)
    }
    env <- parent.env(env)
  }
  FALSE
}


test_that("factory-made functions are enclosed in scrutiny's namespace", {
  for (fn in fns_factory_made) {
    expect_true(inherits_from_scrutiny_ns(fn))
  }
})


test_that("factory-made functions work outside of scrutiny's scope", {
  # This environment stands in for another package's namespace: it has no path
  # to scrutiny's internal helpers except through the manufactured functions
  # themselves.
  foreign <- new.env(parent = baseenv())
  for (name in names(fns_factory_made)) {
    assign(name, fns_factory_made[[name]], envir = foreign)
  }
  assign("df1", tibble::tibble(y = 16:25, n = 3:12), envir = foreign)
  assign("df2", tibble::tibble(y1 = 16:18, y2 = 26:28, n = 12:14), envir = foreign)

  expect_no_error(evalq(map(df1), envir = foreign))
  expect_no_error(evalq(map_seq(df1, dispersion = 1:2), envir = foreign))
  expect_no_error(evalq(map_total_n(df2, dispersion = 1:2), envir = foreign))

  # The results must be the same as when called from within scrutiny's scope:
  expect_equal(
    evalq(map(df1), envir = foreign),
    fns_factory_made$map(tibble::tibble(y = 16:25, n = 3:12))
  )
})


# `function_map_seq()` used to add a `digits_*` column for every reported
# variable except `n`, whether or not `.fun` had a matching argument.
# `audit_seq()` forwards every `digits_*` column back to the mapper as an
# argument, so the mapper rejected its own output.
test_that("`*_map_seq()` only adds `digits_*` columns its mapper accepts", {
  # As in the *Consistency tests in depth* vignette: the basic mapper comes from
  # `function_map()`, so its output carries the class that `audit_seq()` goes by.
  # `audit_seq()` recovers the mapper by evaluating the name from that class, so
  # the mapper has to be reachable from scrutiny's namespace -- which is where a
  # user's own mapper would be, in the global environment.
  assign(
    "schlim_map",
    function_map(
      .fun = schlim_scalar,
      .reported = c("y", "n"),
      .name_test = "SCHLIM"
    ),
    envir = globalenv()
  )
  on.exit(rm("schlim_map", envir = globalenv()), add = TRUE)

  schlim_map_seq <- function_map_seq(
    .fun = get("schlim_map", envir = globalenv()),
    .reported = c("y", "n"),
    .name_test = "SCHLIM"
  )
  out <- schlim_map_seq(tibble::tibble(y = 16:25, n = 3:12))

  # The mapper has no `digits_y` argument, so there must be no such column:
  expect_false(any(grepl("^digits_", colnames(out))))
  expect_no_error(audit_seq(out))

  # The real mappers do have them, and must keep their columns:
  expect_true(
    "digits_x" %in% colnames(grim_map_seq(pigs1, digits_x = 2, dispersion = 1))
  )
  expect_true(
    all(c("digits_x", "digits_sd") %in% colnames(
      grimmer_map_seq(pigs5, digits_x = 2, digits_sd = 2, dispersion = 1)
    ))
  )
})
