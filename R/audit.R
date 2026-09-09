#' Summarize scrutiny objects
#'
#' @description `audit()` summarizes the results of scrutiny functions like
#'   [`grim_map()`] that perform tests on data frames.
#'
#'   See below for a record of such functions. Go to the documentation of any of
#'   them to learn about its `audit()` method.

#' @param data A data frame that inherits one of the classes named below.

#' @details `audit()` is an S3 generic. It looks up the (invisible) scrutiny
#'   class of a tibble returned by any function named below. You don't need to
#'   deal with the classes directly. Behind the scenes, they mediate between
#'   these functions and their associated summary statistics.

#' @section Run before `audit()`:
#'   | \strong{Function}              | \strong{Class}              |
#'   | ---                            | ---                         |
#'   | [`grim_map()`]                 | `"scrutiny_grim_map"`            |
#'   | [`grimmer_map()`]              | `"scrutiny_grimmer_map"`         |
#'   | [`debit_map()`]                | `"scrutiny_debit_map"`           |
#'   | [`duplicate_count()`]          | `"scrutiny_dup_count"`           |
#'   | [`duplicate_count_colpair()`]  | `"scrutiny_dup_count_colpair"`   |
#'   | [`duplicate_tally()`]          | `"scrutiny_dup_tally"`           |
#'   | [`duplicate_detect()`]         | `"scrutiny_dup_detect"`          |
#'   | [`audit_seq()`]                | `"scrutiny_audit_seq"`           |
#'   | [`audit_total_n()`]            | `"scrutiny_audit_total_n"`       |

#' @return A tibble (data frame) with test summary statistics.
#' @export
#'
#' @examples
#' # For basic GRIM-testing:
#' pigs1 |>
#'   grim_map(digits_x = 2) |>
#'   audit()
#'
#' # For duplicate detection:
#' pigs4 |>
#'   duplicate_count() |>
#'   audit()

audit <- function(data) {
  UseMethod("audit")
}


#' Summarize output of sequence mappers and total-n mappers
#'
#' @description `audit_seq()` and `audit_total_n()` summarize the results of
#'   functions that end on `_seq` and `_total_n`, respectively.
#'
#'   See below for a record of such functions. Go to the documentation of any of
#'   them to learn about the way its output is processed by `audit_seq()` or
#'   `audit_total_n()`.

#' @details All functions named below that end on `_seq` were made by
#'   `function_map_seq()`. All that end on `_total_n` were made by
#'   `function_map_total_n()`.

#' @param data A data frame that inherits one of the classes named below.

#' @name audit-special

#' @section Before `audit_seq()`:
#'   | \strong{Function}            | \strong{Class}              |
#'   | ---                          | ---                         |
#'   | `grim_map_seq()`             | `"scrutiny_grim_map_seq"`        |
#'   | `grimmer_map_seq()`          | `"scrutiny_grimmer_map_seq"`     |
#'   | `debit_map_seq()`            | `"scrutiny_debit_map_seq"`       |

#' @section Before `audit_total_n()`:
#'   | \strong{Function}            | \strong{Class}              |
#'   | ---                          | ---                         |
#'   | `grim_map_total_n()`         | `"scrutiny_grim_map_total_n"`    |
#'   | `grimmer_map_total_n()`      | `"scrutiny_grimmer_map_total_n"` |
#'   | `debit_map_total_n()`        | `"scrutiny_debit_map_total_n"`   |

#' @return A tibble (data frame) with test summary statistics.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # For GRIM-testing with dispersed inputs:
#' out <- pigs1 |>
#'   grim_map_seq(digits_x = 2) |>
#'   audit_seq()
#' out
#'
#' # Follow up on `audit_seq()` or
#' # `audit_total_n()` with `audit()`:
#' audit(out)

audit_seq <- function(data) {
  # Functions recovered by name from `data`'s classes are looked up here, in the
  # environment from which `audit_seq()` was called. See `find_fun_by_name()`.
  env_caller <- rlang::caller_env()

  if (!inherits(data, "scrutiny_map_seq")) {
    cli::cli_abort(c(
      "Invalid `data` argument.",
      "!" = "It must be the output of a `*_map_seq()` function, \\
      such as `grim_map_seq()`."
    ))
  }

  check_dispersion_linear(data)

  # `function_map_seq()` records the name of the key result column, which is
  # `"consistency"` unless the mapper was created with a different
  # `.name_key_result`. Subsetting `data` drops the attribute, and every mapper
  # that keeps the default name is unaffected by the fallback:
  name_key_result <- attr(data, "scrutiny_name_key_result", exact = TRUE)
  if (is.null(name_key_result)) {
    name_key_result <- "consistency"
  }

  df_list <- split(data, data$case)

  df_list_hits <- df_list |>
    purrr::map(function(x) x[which(x[[name_key_result]]), ])

  hits_total <- df_list_hits |>
    vapply(nrow, integer(1L), USE.NAMES = FALSE) |>
    unname()

  var_names <- unique(df_list[[1L]]$var)

  # Define some helper functions to be mapped below:
  index_hit_distance <- function(df, var_order = var_names) {
    out <- df |>
      split(df$var) |>
      # `which()` for the same reason as in the `dplyr::filter()` call above,
      # which drops undecidable cases rather than counting them as hits:
      purrr::map(function(x) x[which(x[[name_key_result]]), ])
    # `split()` returns its groups in alphabetical order, and the columns should
    # follow the order of `var` instead. The permutation that undoes a sort is
    # `rank()`, not `order()`: the two are inverses of each other, and they
    # agree only up to three variables that happen not to form a cycle -- with
    # `var = c("sd", "x", "n")` the columns came out in yet a third order.
    out[rank(var_order)] |>
      purrr::map(function(x) x$diff_var)
  }

  length_unless_na <- function(x) {
    if (length(x) == 1L && is.na(x)) {
      0L
    } else {
      length(x)
    }
  }

  # Prepare endings of the `diff_*` columns:
  fun_names <- c("", "_up", "_down")
  fun_names <- rep(fun_names, length(var_names))

  df_nested <- df_list |>
    purrr::map(index_hit_distance) |>
    tibble::tibble(.name_repair = function(x) "distance") |>
    tidyr::unnest_wider(col = distance)

  cols_hits <- df_nested |>
    dplyr::mutate(
      dplyr::across(
        .cols = everything(),
        .fns = function(x) {
          vapply(x, length_unless_na, integer(1L), USE.NAMES = FALSE)
        },
        .names = "hits_{.col}"
      ),
      .keep = "none"
    ) |>
    tidyr::unnest(cols = everything())

  cols_diff <- df_nested |>
    dplyr::mutate(
      dplyr::across(
        .cols = everything(),
        .fns = LIST_MIN_DISTANCE_FUNCTIONS,
        .names = "diff_{.col}{fun_names}"
      ),
      .keep = "none"
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = everything(),
      .fns = function(x) {
        x[is.infinite(x)] <- NA
        as.integer(x)
      }
    )) |>
    suppressWarnings()

  dc <- class(data)
  rounding <- dc[stringr::str_detect(dc, "^scrutiny_rounding_")]
  rounding <- stringr::str_remove(rounding, "^scrutiny_rounding_")

  fun_test_name <- dc[stringr::str_detect(dc, "^scrutiny_.*map$")]
  fun_test_name <- stringr::str_remove(fun_test_name, "^scrutiny_")
  fun_test <- find_fun_by_name(fun_test_name, env_caller)

  data_rev <- reverse_map_seq(data)

  # The `*_map_seq()` output records the arguments that reproduce the test: the
  # `digits_*` values and everything the user passed through the dots, minus
  # helper arguments such as `items`, whose effect is already baked into the key
  # columns. Replaying them keeps the re-test below faithful to the original
  # call. Arguments like `percent`, `threshold`, `symmetric`, or GRIMMER's scale
  # bounds change verdicts but leave no trace in the output columns, so they
  # used to be silently dropped here, which could flip `consistency`:
  args_replay <- attr(data, "scrutiny_fun_args", exact = TRUE)

  if (is.null(args_replay)) {
    # Fallback for output that has lost the attribute, e.g. through subsetting
    # or because it was created by an earlier scrutiny version: the `digits_*`
    # columns and the rounding class are the settings that the output itself
    # records. Each `digits_*` column is constant, so the first value is
    # sufficient; the names match the argument names of `fun_test()`:
    digits_cols <- grep("^digits_", colnames(data), value = TRUE)
    args_replay <- lapply(
      setNames(digits_cols, digits_cols),
      function(col) data[[col]][[1L]]
    )
    if (length(rounding) > 0L) {
      args_replay$rounding <- rounding
    }
  }

  data_rev_tested <- do.call(fun_test, c(list(data_rev), args_replay))

  consistency <- data_rev_tested[[name_key_result]]

  cols_hits <- dplyr::mutate(
    cols_hits,
    dplyr::across(
      .cols = where(is.character),
      .fns = as.numeric
    )
  )

  cols_diff <- dplyr::mutate(
    cols_diff,
    dplyr::across(
      .cols = where(is.character),
      .fns = as.numeric
    )
  )

  data_rev |>
    dplyr::mutate("{name_key_result}" := consistency, hits_total) |>
    dplyr::bind_cols(cols_hits, cols_diff) |>
    add_class("scrutiny_audit_seq")
}


#' @rdname audit-special
#' @export

audit_total_n <- function(data) {
  if (!inherits(data, "scrutiny_map_total_n")) {
    cli::cli_abort(c(
      "Invalid `data` argument.",
      "!" = "It must be the output of a `*_map_total_n()` function, \\
      such as `grim_map_total_n()`."
    ))
  }

  df_list <- split(data, data$case)

  df_list_hits <- df_list |>
    purrr::map(dplyr::filter, both_consistent)

  map_nrow_half <- function(x) {
    vapply(x, nrow, integer(1L), USE.NAMES = FALSE) / 2L
  }

  hits_forth <- df_list_hits |>
    purrr::map(dplyr::filter, dir == "forth") |>
    map_nrow_half()

  hits_back <- df_list_hits |>
    purrr::map(dplyr::filter, dir == "back") |>
    map_nrow_half()

  hits_total <- hits_forth + hits_back
  scenarios_total <- map_nrow_half(df_list)
  hit_rate <- hits_total / scenarios_total

  data |>
    reverse_map_total_n() |>
    dplyr::mutate(
      hits_total,
      hits_forth,
      hits_back,
      scenarios_total,
      hit_rate,
      dplyr::across(
        .cols = c("n", starts_with("hits"), "scenarios_total"),
        .fns = as.integer
      )
    ) |>
    add_class("scrutiny_audit_total_n")
}


# `audit_seq()` needs to call the mapper that produced its input, but all it has
# to go by is the mapper's name, recovered from a class such as
# `"scrutiny_grim_map"`. Evaluating that name from inside `audit_seq()` would
# search scrutiny's namespace, so a user's own mapper was only ever found if it
# happened to live in the global environment. Searching `env` -- the environment
# from which `audit_seq()` was called -- finds mappers wherever they are
# defined: in another package's namespace, in a local scope, or in a test block.
# scrutiny's own namespace is the fallback for callers that can't see scrutiny,
# as when `audit_seq()` is called via `scrutiny::`.

find_fun_by_name <- function(name, env) {
  fun <- get0(name, envir = env, mode = "function")

  if (is.null(fun)) {
    fun <- get0(name, envir = asNamespace("scrutiny"), mode = "function")
  }

  if (is.null(fun)) {
    cli::cli_abort(c(
      "Can't find the function `{name}()`.",
      "x" = "`data` has the {.cls scrutiny_{name}} class, so it should have \\
      been created by `{name}()`.",
      "i" = "Make sure `{name}()` can be found from where you call \\
      `audit_seq()`."
    ))
  }

  fun
}
