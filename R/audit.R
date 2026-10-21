#' Summarize scrutiny objects
#'
#' @description `audit()` summarizes the output of scrutiny functions that test
#'   data frames, such as [`grim_map()`]. It is an S3 generic with methods for
#'   the functions listed below. The columns of each summary are described in
#'   the documentation of the respective function.
#'
#' @param data A data frame returned by one of the functions below.
#'
#' @section Run before `audit()`:
#'   | \strong{Function}              | \strong{Class}                      |
#'   | ---                            | ---                                 |
#'   | [`grim_map()`]                 | `"scrutiny_grim_map"`               |
#'   | [`grimmer_map()`]              | `"scrutiny_grimmer_map"`            |
#'   | [`debit_map()`]                | `"scrutiny_debit_map"`              |
#'   | [`is_percentage_of_map()`]     | `"scrutiny_is_percentage_of_map"`   |
#'   | [`add_up_to_map()`]            | `"scrutiny_add_up_to_map"`          |
#'   | [`duplicate_count()`]          | `"scrutiny_dup_count"`              |
#'   | [`duplicate_count_colpair()`]  | `"scrutiny_dup_count_colpair"`      |
#'   | [`duplicate_tally()`]          | `"scrutiny_dup_tally"`              |
#'   | [`duplicate_detect()`]         | `"scrutiny_dup_detect"`             |
#'   | [`audit_seq()`]                | `"scrutiny_audit_seq"`              |
#'   | [`audit_total_n()`]            | `"scrutiny_audit_total_n"`          |
#'
#' @return A tibble with summary statistics.
#' @export
#'
#' @examples
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1) |>
#'   audit()
#'
#' pigs4 |>
#'   duplicate_count() |>
#'   audit()

audit <- function(data) {
  UseMethod("audit")
}


#' Summarize output of sequence mappers and total-n mappers
#'
#' @description `audit_seq()` summarizes the output of functions made by
#'   [`function_map_seq()`], such as [`grim_map_seq()`], and `audit_total_n()`
#'   that of functions made by [`function_map_total_n()`], such as
#'   [`grim_map_total_n()`]. The columns of each summary are described in the
#'   documentation of the respective function.
#'
#' @param data A data frame returned by one of the functions below.
#'
#' @name audit-special
#'
#' @section Before `audit_seq()`:
#'   | \strong{Function}            | \strong{Class}                   |
#'   | ---                          | ---                              |
#'   | [`grim_map_seq()`]           | `"scrutiny_grim_map_seq"`        |
#'   | [`grimmer_map_seq()`]        | `"scrutiny_grimmer_map_seq"`     |
#'   | [`debit_map_seq()`]          | `"scrutiny_debit_map_seq"`       |
#'   | [`is_percentage_of_map_seq()`] | `"scrutiny_is_percentage_of_map_seq"` |
#'
#' @section Before `audit_total_n()`:
#'   | \strong{Function}            | \strong{Class}                   |
#'   | ---                          | ---                              |
#'   | [`grim_map_total_n()`]       | `"scrutiny_grim_map_total_n"`    |
#'   | [`grimmer_map_total_n()`]    | `"scrutiny_grimmer_map_total_n"` |
#'   | [`debit_map_total_n()`]      | `"scrutiny_debit_map_total_n"`   |
#'
#' @return A tibble with summary statistics.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' out <- pigs1 |>
#'   grim_map_seq(digits_x = 2, items_averaged = 1) |>
#'   audit_seq()
#' out
#'
#' # `audit()` summarizes the summary:
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

  check_dispersion_linear(data, "audit_seq")

  # `function_map_seq()` records the name of the key result column, which is
  # `"consistency"` unless the mapper was created with a different
  # `.name_key_result`. Output that has lost the attribute falls back to that,
  # and every mapper that keeps the default name is unaffected:
  meta <- scrutiny_meta(data)
  name_key_result <- meta$name_key_result
  if (is.null(name_key_result)) {
    name_key_result <- "consistency"
  }

  # With no inconsistent case, there is nothing to count hits in, but the
  # output has the columns of any other. The dispersed variables are read off
  # the attribute, since the `var` column has no values; output that has lost
  # the attribute gets no per-variable columns:
  if (nrow(data) == 0L) {
    var_names <- meta$var
    out <- reverse_map_seq(data)
    out[[name_key_result]] <- logical(0L)
    out$hits_total <- integer(0L)
    names_var_cols <- c(
      paste0("hits_", var_names),
      paste0("diff_", rep(var_names, each = 3L), c("", "_up", "_down"))
    )
    for (name in names_var_cols) {
      out[[name]] <- integer(0L)
    }
    return(add_class(out, "scrutiny_audit_seq"))
  }

  df_list <- split(data, data$case)

  df_list_hits <- df_list |>
    purrr::map(function(x) x[which(x[[name_key_result]]), ])

  # The reported value set itself, included under `include_reported = TRUE`, is
  # one value set, however many variables it is listed under:
  hits_total <- df_list_hits |>
    vapply(
      function(x) sum(x$diff_var != 0L) + any(x$diff_var == 0L),
      integer(1L),
      USE.NAMES = FALSE
    ) |>
    unname()

  # The dispersed variables, in the order of the `var` argument. The attribute
  # records them all, even one whose dispersion `out_min` or `out_max` clipped
  # away in every case; the `var` column only has those with rows:
  var_names <- meta$var
  if (is.null(var_names)) {
    var_names <- unique(data$var)
  }

  # Define some helper functions to be mapped below:
  index_hit_distance <- function(df, var_order = var_names) {
    # Splitting by a factor keeps the groups in the order of `var` rather than
    # the alphabetical one, and it keeps an empty group for a variable that has
    # no rows in this case because `out_min` or `out_max` clipped its whole
    # dispersion. Such a group counts as zero hits, like one whose rows are all
    # inconsistent. Without it, the columns of later variables shifted left:
    df |>
      split(factor(df$var, levels = var_order)) |>
      # `which()` for the same reason as in the `dplyr::filter()` call above,
      # which drops undecidable cases rather than counting them as hits:
      purrr::map(function(x) x[which(x[[name_key_result]]), ]$diff_var)
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

  fun_test_name <- data |>
    class() |>
    call_on(\(x) x[stringr::str_detect(x, "^scrutiny_.*map$")]) |>
    stringr::str_remove("^scrutiny_")

  fun_test <- find_fun_by_name(fun_test_name, env_caller)
  data_rev <- reverse_map_seq(data)

  # The `*_map_seq()` output records the arguments that reproduce the test: the
  # `digits_*` values and everything the user passed through the dots, minus
  # helper arguments such as `items_averaged`, which are columns of the output,
  # and so of the reconstructed data. Replaying them keeps the re-test below
  # faithful to the original call. Arguments like `percent`, `threshold`,
  # `symmetric`, or GRIMMER's scale bounds change verdicts but leave no trace in
  # the output columns, so they used to be silently dropped here, which could
  # flip `consistency`:
  args_replay <- meta$fun_args

  if (is.null(args_replay)) {
    # Fallback for output that has lost the attribute, or that was created by
    # an earlier scrutiny version: the `digits_*` columns are the settings
    # that the output itself records. Each of them is constant, so the first
    # value is sufficient; the names match the argument names of `fun_test()`:
    digits_cols <- grep("^digits_", colnames(data), value = TRUE)
    args_replay <- digits_cols |>
      setNames(digits_cols) |>
      lapply(function(col) data[[col]][[1L]])
  }

  data_rev_tested <- do.call(fun_test, c(list(data_rev), args_replay))

  consistency <- data_rev_tested[[name_key_result]]

  cols_hits <- cols_hits |>
    dplyr::mutate(dplyr::across(
      .cols = where(is.character),
      .fns = as.numeric
    ))

  cols_diff <- cols_diff |>
    dplyr::mutate(dplyr::across(
      .cols = where(is.character),
      .fns = as.numeric
    ))

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
    abort_in_export(
      "Can't find the function `{name}()`.",
      "x" = "`data` has the {.cls scrutiny_{name}} class, so it should have \\
      been created by `{name}()`.",
      "i" = "Make sure `{name}()` can be found from where you call \\
      `audit_seq()`."
    )
  }

  fun
}
