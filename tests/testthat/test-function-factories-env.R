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
  data |> dplyr::mutate(consistency)
}

fns_factory_made <- list(
  map = function_map(
    .fn = schlim_scalar,
    .reported = c("y", "n"),
    .name_test = "SCHLIM"
  ),
  map_seq = function_map_seq(
    .fn = schlim_map,
    .reported = c("y", "n"),
    .name_test = "SCHLIM"
  ),
  map_total_n = function_map_total_n(
    .fn = schlim_map,
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
    fn |> inherits_from_scrutiny_ns() |> expect_true()
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

  # Not aligning pipes here because the lengths are too different
  quote(map(df1)) |> eval(foreign) |> expect_no_error()
  quote(map_seq(df1, dispersion = 1:2)) |> eval(foreign) |> expect_no_error()
  quote(map_total_n(df2, dispersion = 1:2)) |> eval(foreign) |> expect_no_error()

  # The results must be the same as when called from within scrutiny's scope:
  expected <- tibble::tibble(y = 16:25, n = 3:12) |> fns_factory_made$map()
  quote(map(df1)) |> eval(foreign) |> expect_equal(expected)
})


# `function_map_seq()` used to add a `digits_*` column for every reported
# variable except `n`, whether or not `.fn` had a matching argument.
# `audit_seq()` forwards every `digits_*` column back to the mapper as an
# argument, so the mapper rejected its own output.
test_that("`*_map_seq()` only adds `digits_*` columns its mapper accepts", {
  # As in the *Consistency tests in depth* vignette: the basic mapper comes from
  # `function_map()`, so its output carries the class that `audit_seq()` goes
  # by. `audit_seq()` recovers the mapper by that name, looking it up in the
  # environment it was called from -- here, this test block.
  schlim_map <- function_map(
    .fn = schlim_scalar,
    .reported = c("y", "n"),
    .name_test = "SCHLIM"
  )

  schlim_map_seq <- function_map_seq(
    .fn = schlim_map,
    .reported = c("y", "n"),
    .name_test = "SCHLIM"
  )
  out <- tibble::tibble(y = 16:25, n = 3:12) |> schlim_map_seq()

  # The mapper has no `digits_y` argument, so there must be no such column:
  out |> colnames() |> grepl(pattern = "^digits_") |> any() |> expect_false()
  out |> audit_seq() |> expect_no_error()

  # The real mappers do have them, and must keep their columns:
  pigs1 |>
    grim_map_seq(digits_x = 2, items_averaged = 1, dispersion = 1) |>
    colnames() |>
    expect_contains("digits_x")
  pigs5 |>
    grimmer_map_seq(digits_x = 2, digits_sd = 2, items_averaged = 1, dispersion = 1) |>
    colnames() |>
    expect_contains(c("digits_x", "digits_sd"))
})


# The proto function used to repeat the other key columns through list-columns
# and `tidyr::unnest_longer()`, which left a 0-column `data_seq` with the wrong
# number of rows when there were no other key columns.
test_that("`function_map_seq()` works for a mapper with one reported variable", {
  even_map <- function_map(
    .fn = function(x) x %% 2 == 0,
    .reported = "x",
    .name_test = "EVEN"
  )
  even_map_seq <- function_map_seq(
    .fn = even_map,
    .reported = "x",
    .name_test = "EVEN"
  )
  out <- tibble::tibble(x = c(3, 8)) |> even_map_seq(dispersion = 1:2)
  out$x |> expect_equal(c(1, 2, 4, 5))
  out$consistency |> expect_equal(c(FALSE, TRUE, TRUE, FALSE))
  out$case |> expect_equal(rep(1L, 4))
})


# `audit_seq()` used to recover the mapper that produced its input by the name
# implied by a class such as `"scrutiny_schlim_map"` and call it again. It now
# reads the verdicts off the attribute, so the mapper need not be findable at
# all.
test_that("`audit_seq()` works on a mapper that nothing can find any more", {
  # Local to this function, so neither scrutiny's namespace nor the global
  # environment has any path to it:
  make_out_seq <- function() {
    schlim_map <- function_map(
      .fn = schlim_scalar,
      .reported = c("y", "n"),
      .name_test = "SCHLIM"
    )
    schlim_map_seq <- function_map_seq(
      .fn = schlim_map,
      .reported = c("y", "n"),
      .name_test = "SCHLIM"
    )
    tibble::tibble(y = 16:25, n = 3:12) |> schlim_map_seq() |> audit_seq()
  }

  "schlim_map" |> exists(envir = globalenv(), inherits = FALSE) |> expect_false()
  make_out_seq() |> expect_no_error()
})


test_that("`audit_seq()` works from a foreign caller", {
  # Standing in for a caller that has no path to scrutiny, as when `audit_seq()`
  # is called via `scrutiny::audit_seq()`:
  foreign <- new.env(parent = baseenv())
  assign("audit_seq", audit_seq, envir = foreign)
  out_seq <- pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1)
  assign("out", out_seq, envir = foreign)

  expected <- out_seq |> audit_seq()
  quote(audit_seq(out)) |> eval(foreign) |> expect_equal(expected)
})


test_that("`audit_seq()` errors informatively if the attribute is lost", {
  out <- pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1)
  attr(out, "scrutiny") <- NULL
  out |> audit_seq() |> expect_error("lost the settings")

  # The mapper's name is not needed, so a class nobody can resolve is no
  # problem:
  out <- pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1)
  class(out)[class(out) == "scrutiny_grim_map"] <- "scrutiny_nonexistent_map"
  out |> audit_seq() |> expect_no_error()
})
