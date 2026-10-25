#' Map a single-case function over its caller's arguments
#'
#' `map_value_sets()` applies `fun` once per value set, taking the arguments
#' of the function that calls it, such as `grim()`, which writes out its own
#' signature mirroring that of the `*_scalar()` function it wraps -- minus the
#' internal-only `show_rec` / `show_reason` (see gotcha 20 in AGENTS.md):
#'
#' ```
#' grim <- function(x, n, digits_x, items_averaged, percent = FALSE, ...) {
#'   map_value_sets(grim_scalar, c("x", "n", "digits_x", "items_averaged"))
#' }
#' ```
#'
#' @param fun The `*_scalar()` function to apply, e.g. `grim_scalar()`.
#' @param along String vector naming the arguments to vectorize over: the key
#'   arguments, their `digits_*` arguments, and `items_averaged` where the test
#'   has it. Every other argument is scalar. The split is the same one the
#'   mapper tier makes: `along` is what `function_map()` takes from `data` --
#'   key columns, `.args_by_row`, and helper columns -- and the rest are what it
#'   passes as constants for the whole call.
#' @param type String. The type of the return value: `"logical"` for the
#'   tests, `"double"` for `grim_closest()`, `"list"` for `grim_values()`,
#'   whose `fun` returns a vector of any length per value set.
#'
#' @return Vector of `type`, as long as the recycled `along` arguments.
#'
#' @noRd
map_value_sets <- function(fun, along, type = "logical") {
  frame <- parent.frame()
  formals_wrapper <- formals(sys.function(sys.parent()))
  args_all <- names(formals_wrapper)

  # Arguments the caller left out are not forwarded at all, so `fun` applies its
  # own defaults -- the same ones the wrapper states -- and raises its own error
  # where it has none. That covers three cases with one rule: `digits_x` and
  # `digits_sd`, which must arrive missing so that `error_digits_missing()`
  # fires; the ordinary defaults; and `tolerance`, whose
  # `lifecycle::deprecated()` default is itself a sentinel that must not be
  # forced.
  #
  # `missing()` per formal, evaluated in the wrapper's frame. `match.call()`
  # would answer in one step but cannot be trusted here: a wrapper reached
  # through `lapply()` is called as `FUN(X[[i]], ...)`, and matching that fails
  # with "... used in a situation where it does not exist". The calls are built
  # once per signature; see `missing_arg_calls()`.
  calls_missing <- missing_arg_calls(args_all)

  is_supplied <- !vapply(
    calls_missing,
    eval,
    logical(1L),
    envir = frame,
    USE.NAMES = FALSE
  )

  args_supplied <- args_all[is_supplied]
  vals <- mget(args_supplied, envir = frame)

  names_along <- along[along %in% args_supplied]
  names_scalar <- args_supplied[!args_supplied %in% along]

  # Arguments that say *how* to test rather than *what* describe the call as a
  # whole: one call cannot use two rounding methods, and `x` is either a
  # percentage or it isn't. `reround()` has treated them as scalar throughout.
  for (name in names_scalar) {
    value <- vals[[name]]
    if (length(value) != 1L) {
      name_mapper <- paste0(name_last_export(), "_map")
      abort_in_export(
        "{.arg {name}} must be length 1, not {length(value)}.",
        "x" = "It describes the test as a whole, so it cannot vary from \\
        one value set to the next.",
        "i" = "Only {.arg {along}} are vectorized.",
        "i" = if (exists(name_mapper, mode = "function")) {
          "To test value sets that differ in {.arg {name}}, call \\
          {.fun {name_mapper}} once for each of its values."
        }
      )
    }
  }

  # A required argument is missing, so hand the call to `fun` as it stands and
  # let it raise its error -- here rather than inside the loop, so that the error
  # is attributed to the wrapper and raised once.
  for (name in args_all[!args_all %in% args_supplied]) {
    if (identical(formals_wrapper[[name]], quote(expr = ))) {
      return(do.call(fun, vals))
    }
  }

  # A `NULL` is no value set, so `digits_x = NULL` used to return `logical(0)`
  # without a word:
  for (name in names_along) {
    if (is.null(vals[[name]])) {
      abort_in_export("{.arg {name}} must not be `NULL`.")
    }
  }

  lengths_along <- lengths(vals[names_along])
  length_out <- recycle_length_common(lengths_along, names_along)

  if (length_out == 0L) {
    return(vector(type, 0L))
  }

  vals_along <- vals[names_along]

  for (name in names_along[lengths_along != length_out]) {
    vals_along[[name]] <- rep_len(vals_along[[name]], length_out)
  }

  # `.mapply()` is `mapply()` without the argument matching and the
  # `simplify2array()` that turned a list return into a matrix. Recycling has
  # already happened above, so it has nothing to do but loop.
  results <- .mapply(fun, vals_along, MoreArgs = vals[names_scalar])

  out <- if (type == "list") results else unlist(results, use.names = FALSE)

  # The whole point of the wrappers is that they return a vector of `type`, one
  # value per value set, whatever the arguments. If a `*_scalar()` function
  # ever returned something else, that would be a bug in scrutiny rather than
  # in the call, so this reports it as one:
  if (typeof(out) != type || length(out) != length_out) {
    cli::cli_abort(
      c(
        "Internal error: `fun` returned {length(out)} \\
      {.cls {typeof(out)}} value{?s} for {length_out} value set{?s}.",
        "i" = "It must return one {type} value per value set."
      ),
      .internal = TRUE
    )
  }

  out
}


#' `missing()` calls for a wrapper's formals
#'
#' Returns `list(quote(missing(x)), quote(missing(n)), ...)`, to be evaluated
#' in the wrapper's frame. The list depends only on the formals' names, so it
#' is cached by them. Building it per call costs about as much as evaluating
#' it, and a `grim()` called one value set at a time pays that on every call.
#'
#' @param args_all String vector. Names of the wrapper's formals.
#'
#' @return List of calls.
#'
#' @noRd
missing_arg_calls <- function(args_all) {
  key <- paste(args_all, collapse = ",")
  calls <- cache_missing_arg_calls[[key]]

  if (is.null(calls)) {
    calls <- lapply(args_all, function(name) call("missing", as.name(name)))
    cache_missing_arg_calls[[key]] <- calls
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
#'
#' @return Integer (length 1).
#'
#' @noRd
recycle_length_common <- function(lengths_args, names_args) {
  lengths_free <- lengths_args[lengths_args != 1L]

  if (length(lengths_free) == 0L) {
    return(if (length(lengths_args) == 0L) 0L else 1L)
  }

  length_out <- max(lengths_free)
  is_offender <- lengths_args != 1L & lengths_args != length_out

  if (any(is_offender)) {
    name_first <- names_args[lengths_args == length_out][1L]
    names_offenders <- names_args[is_offender]
    abort_in_export(
      "Can't recycle {.arg {name_first}} (length {length_out}) to match \\
      {.arg {names_offenders}} (length {lengths_args[is_offender]}).",
      "i" = "Arguments that are tested for consistency with each other \\
      must have the same length, or length 1."
    )
  }

  as.integer(length_out)
}
