#' Vectorize a single-case consistency test
#'
#' `vectorize_test()` is the shared body of the vectorized consistency test
#' functions -- `grim()`, `grimmer()`, and `debit()`. Each of them writes out
#' its own signature, mirroring that of the `*_scalar()` function it wraps
#' minus the internal-only `show_rec` / `show_reason` argument, and hands its
#' frame to `vectorize_test()`:
#'
#' ```
#' grim <- function(x, n, digits_x, items = 1, percent = FALSE, ...) {
#'   vectorize_test(grim_scalar, environment(), c("x", "n", "digits_x", "items"))
#' }
#' ```
#'
#' Up to scrutiny 1.0.0 the wrappers were `Vectorize(grim_scalar)` and the
#' like. That copied *all* of the scalar function's formals, including
#' `show_rec` and `show_reason`, which are internal to the mapper tier and were
#' documented as such -- and, because `mapply()` simplifies a list return, a
#' user who set one got an unnamed matrix of lists rather than the logical
#' vector the function promises. Writing the signatures out is what removes
#' those arguments from the exported functions; this helper is what keeps the
#' three bodies from being three copies of the same loop.
#'
#' @param .fun The `*_scalar()` function to apply, e.g. `grim_scalar()`.
#' @param .frame The calling wrapper's own `environment()`.
#' @param .along String vector naming the arguments to vectorize over: the key
#'   arguments, their `digits_*` arguments, and `items` where the test has it.
#'   Every other argument is scalar. The split is the same one the mapper tier
#'   makes: `.along` is what `function_map()` takes from `data` -- key columns,
#'   `.args_by_row`, and helper columns -- and the rest are what it passes as
#'   constants for the whole call.
#'
#' @return Logical vector, as long as the recycled `.along` arguments.
#'
#' @noRd
vectorize_test <- function(.fun, .frame, .along) {
  index_wrapper <- sys.parent()
  fn_wrapper <- sys.function(index_wrapper)
  formals_wrapper <- formals(fn_wrapper)
  args_all <- names(formals_wrapper)

  # The call the user typed, e.g. `grim(x = 5.19, n = 28)`. Errors below are
  # attributed to it, not to the present helper, which the user has no way of
  # knowing about:
  call_wrapper <- sys.call(index_wrapper)
  name_wrapper <- fn_name_from_call(call_wrapper)

  # Arguments the caller left out are not forwarded at all. `.fun` then applies
  # its own default, which is the same one the wrapper states; and where it has
  # none, it raises its own error rather than receiving a value that R would
  # have to invent. That covers three cases with one rule: `digits_x` and
  # `digits_sd`, which must reach `.fun` as missing so that
  # `error_digits_missing()` fires instead of R's generic message; the ordinary
  # defaults; and `tolerance`, whose `lifecycle::deprecated()` default is
  # itself a missing-argument sentinel that must not be forced.
  #
  # `missing()` is asked about each formal in turn, evaluated in the wrapper's
  # frame. `match.call()` on the wrapper's call would answer the same question
  # in one step, but it cannot be trusted here: a wrapper reached through
  # `lapply()` or `Map()` is called as `FUN(X[[i]], ...)`, and matching that
  # call fails with "... used in a situation where it does not exist". The
  # `missing()` calls themselves are built once per wrapper, not once per call;
  # see `missing_arg_calls()`.
  calls_missing <- missing_arg_calls(name_wrapper, args_all)

  is_supplied <- !vapply(
    calls_missing,
    eval,
    logical(1L),
    envir = .frame,
    USE.NAMES = FALSE
  )

  args_supplied <- args_all[is_supplied]
  vals <- mget(args_supplied, envir = .frame)

  names_along <- .along[.along %in% args_supplied]
  names_scalar <- args_supplied[!args_supplied %in% .along]

  # Arguments that say *how* to test rather than *what* to test describe the
  # call as a whole: one call cannot use two rounding methods, and `x` is
  # either a percentage or it isn't. `Vectorize()` looped over them along with
  # everything else, so `grim(x = c(5.19, 5.19), n = 28, digits_x = 2,
  # rounding = c("up", "down"))` returned one verdict per rounding method.
  # `reround()` has treated them as scalar throughout.
  for (name in names_scalar) {
    value <- vals[[name]]
    # `NULL` is a value some of them take: GRIMMER's `min_val` and `max_val`
    # are `NULL` for an unbounded scale.
    if (!is.null(value) && length(value) != 1L) {
      name_mapper <- paste0(name_wrapper, "_map")
      # Not `.along` directly: cli reads a `{}` expression that starts with a
      # dot as one of its own styles.
      names_vectorized <- .along
      cli::cli_abort(
        c(
          "{.arg {name}} must be length 1, not {length(value)}.",
          "x" = "It describes the test as a whole, so it cannot vary from \\
          one value set to the next.",
          "i" = "Only {.arg {names_vectorized}} are vectorized.",
          "i" = "To test value sets that differ in {.arg {name}}, call \\
          {.fun {name_mapper}} once for each of its values."
        ),
        call = call_wrapper
      )
    }
  }

  # A required argument was not supplied, so there is no point in working out
  # how many times to apply the test: hand the call to `.fun` as it stands and
  # let it raise its error. Doing so here rather than inside the loop keeps the
  # error attributed to the wrapper the user called, and keeps it from being
  # raised once per element.
  for (name in args_all[!args_all %in% args_supplied]) {
    if (identical(formals_wrapper[[name]], quote(expr = ))) {
      return(do.call(.fun, vals))
    }
  }

  lengths_along <- lengths(vals[names_along])
  length_out <- recycle_length_common(
    lengths_along,
    names_along,
    call = call_wrapper
  )

  if (length_out == 0L) {
    return(logical(0L))
  }

  vals_along <- vals[names_along]

  for (name in names_along[lengths_along != length_out]) {
    vals_along[[name]] <- rep_len(vals_along[[name]], length_out)
  }

  # `.mapply()` is the internal workhorse behind `mapply()`, without the
  # argument matching and the `simplify2array()` that made a list return come
  # back as a matrix. Recycling has already happened above, so it has nothing
  # left to do but loop.
  results <- .mapply(.fun, vals_along, MoreArgs = vals[names_scalar])

  out <- unlist(results, use.names = FALSE)

  # The whole point of the wrappers is that they return a logical vector, one
  # value per value set, whatever the arguments. If a `*_scalar()` function
  # ever returned something else, that would be a bug in scrutiny rather than
  # in the call, so this reports it as one:
  if (!is.logical(out) || length(out) != length_out) {
    cli::cli_abort(
      c(
        "Internal error: the consistency test returned {length(out)} \\
      {.cls {typeof(out)}} value{?s} for {length_out} value set{?s}.",
        "i" = "It must return one logical value per value set."
      ),
      .internal = TRUE
    )
  }

  out
}


#' `missing()` calls for a wrapper's formals
#'
#' Returns `list(quote(missing(x)), quote(missing(n)), ...)` for the formals of
#' the vectorized test function named `name_wrapper`, to be evaluated in that
#' function's frame.
#'
#' The list depends only on the wrapper's signature, which is fixed at build
#' time, so it is built once per wrapper and cached. Building it per call costs
#' about as much as evaluating it, and a `grim()` called one value set at a
#' time pays that on every call.
#'
#' The cache is keyed by the name the call was made under, which is not by
#' itself proof of anything: a user can bind `grim` and then `grimmer` to the
#' same name. The formals are therefore stored alongside the calls and compared
#' before a hit is accepted.
#'
#' @param name_wrapper String (length 1). Name the wrapper was called under.
#' @param args_all String vector. Names of its formals.
#'
#' @return List of calls.
#'
#' @noRd
missing_arg_calls <- function(name_wrapper, args_all) {
  # `fn_name_from_call()` returns the empty string for a caller that has no
  # name of its own, as when `do.call()` is given the function object. That is
  # no key at all, and an environment cannot even be subset by it:
  has_key <- nzchar(name_wrapper)

  if (has_key) {
    entry <- cache_missing_arg_calls[[name_wrapper]]
    if (!is.null(entry) && identical(entry$args, args_all)) {
      return(entry$calls)
    }
  }

  calls <- lapply(args_all, function(name) call("missing", as.name(name)))

  if (has_key) {
    cache_missing_arg_calls[[name_wrapper]] <- list(
      args = args_all,
      calls = calls
    )
  }

  calls
}

cache_missing_arg_calls <- new.env(parent = emptyenv())


#' Common length of arguments that are recycled against each other
#'
#' Tidyverse recycling rules: a length-1 argument is recycled to the length of
#' the others, and everything else must already have that length. `mapply()`
#' recycled anything into anything and only warned when the longer length was
#' not a multiple of the shorter, so that `grim(x = <3 values>, n = <2 values>)`
#' returned three verdicts, the third of which paired the third `x` with the
#' first `n`.
#'
#' @param lengths_args Integer vector of argument lengths.
#' @param names_args String vector of the corresponding argument names.
#' @param call The call to attribute an error to.
#'
#' @return Integer (length 1).
#'
#' @noRd
recycle_length_common <- function(lengths_args, names_args, call = NULL) {
  lengths_free <- lengths_args[lengths_args != 1L]

  if (length(lengths_free) == 0L) {
    return(if (length(lengths_args) == 0L) 0L else 1L)
  }

  length_out <- max(lengths_free)
  is_offender <- lengths_args != 1L & lengths_args != length_out

  if (any(is_offender)) {
    name_first <- names_args[lengths_args == length_out][1L]
    names_offenders <- names_args[is_offender]
    cli::cli_abort(
      c(
        "Can't recycle {.arg {name_first}} (length {length_out}) to match \\
        {.arg {names_offenders}} (length {lengths_args[is_offender]}).",
        "i" = "Arguments that are tested for consistency with each other \\
        must have the same length, or length 1."
      ),
      call = call
    )
  }

  as.integer(length_out)
}
