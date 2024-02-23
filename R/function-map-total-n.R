
# Helper function used within `function_map_total_n_proto()` below; not
# exported:
mutate_both_consistent <- function(data) {
  both_consistent <- data$consistency %>%
    split_into_groups(group_size = 2) %>%
    vapply(all, logical(1L), USE.NAMES = FALSE) %>%
    rep(each = 2L)

  dplyr::mutate(data, both_consistent, .after = "consistency")
}



# # Example for `reported` and `data`:
# reported <- tibble(
#   x1  = c("0.34", "0.42", "0.50"),
#   x2  = c("0.37", "0.45", "0.53"),
#   sd1 = c("0.21", "0.31", "0.29"),
#   sd2 = c("0.18", "0.30", "0.28")
# )
#
# data <- reported %>%
#   dplyr::mutate(n = c(90, 103, 84))
#
# dir <- "forth"
# fun <- debit_map
#
# n <- 35
# dispersion <- 0:5
# reported_orig <- c("x", "sd")
# n_min <- 1
# n_max <- NULL
# x1 <- NA
# x2 <- NA

# # Example for `df_list` (if needed):
# df_list <- list(
#   disperse(45),
#   disperse2(c(51, 52))[1:10, ],
#   disperse(20)[1:8, ]
# )


# Used within `function_map_total_n()`:
function_map_total_n_proto <- function(.fun, .reported, .reported_orig, .dir,
                                       .dispersion = 0:5,
                                       .n_min = 1L, .n_max = NULL,
                                       .constant = NULL, ...) {

  function(data, fun = .fun, reported = .reported,
           reported_orig = .reported_orig, dir = .dir,
           dispersion = .dispersion,
           n_min = .n_min, n_max = .n_max,
           constant = .constant, ...) {

    reported_n_cols <- ncol(reported)
    reported_n_vars <- reported_n_cols / 2

    df_list <- data %>%
      dplyr::select(n) %>%
      purrr::pmap(
        disperse_total, dispersion = dispersion, n_min = n_min, n_max = n_max,
        constant = constant
      )

    # Row numbers of `disperse()` tibbles will be used below to determine how
    # often the reported values need to be repeated:
    df_list_nrow <- vapply(df_list, nrow, integer(1L), USE.NAMES = FALSE)
    df_list_n_groups <- length(df_list_nrow)

    out_df_nested <- reported %>%
      split_into_rows() %>%
      purrr::map(split_into_groups, group_size = 2) %>%
      tibble::tibble(.name_repair = function(x) "reported") %>%
      dplyr::mutate(
        df_list,
        times    = df_list_nrow / 2,
        reported = purrr::map2(
          reported, times, function(x, y) purrr::map2(x, y, rep)
        )
      ) %>%
      tidyr::unnest_wider(reported) %>%
      dplyr::rename_with(
        .fn   = function(x) reported_orig,
        .cols = 1L:dplyr::all_of(reported_n_vars)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = 1L:dplyr::all_of(reported_n_vars),
          .fns  = function(x) purrr::map(x, tibble::as_tibble)
        ), times = NULL
      )

    # Rename the tibbles nested within the list-columns using the original names
    # of the reported variables (e.g., `c("x", "sd")`) so that it's clear what
    # those values represent:
    out_df_nested[1L:reported_n_vars] <-
      out_df_nested[1L:reported_n_vars] %>%
      tidyr::pivot_longer(cols = everything()) %>%
      dplyr::mutate(value = purrr::map2(value, name, setNames)) %>%
      tidyr::pivot_wider(
        names_from  = name,
        values_from = value,
        values_fn   = list
      ) %>%
      tidyr::unnest(cols = everything())

    # This references the rows in the `reported` data frame to which the various
    # scenarios belong. In other words, `case` is identical to the respective
    # row number in `reported`:
    case <- df_list_nrow %>%
      purrr::map2(1L:df_list_n_groups, ., rep) %>%
      purrr::flatten_int()

    out_df <- tidyr::unnest(out_df_nested, cols = everything())
    n_change <- out_df$n_change
    colnames(out_df)[seq_along(reported_orig)] <- reported_orig

    out_df <- fun(out_df, ...)

    if (!any("n_change" == colnames(out_df))) {
      out_df <- dplyr::mutate(out_df, n_change)
    }

    out_df <- out_df %>%
      mutate_both_consistent() %>%
      dplyr::mutate(case, dir, n = as.integer(n)) %>%
      dplyr::relocate(n_change, .after = n)

    return(out_df)
  }

  # -- End of the manufactured function --

}




#' Create new `*_map_total_n()` functions
#'
#' @description `function_map_total_n()` is the engine that powers functions
#'   such as [`grim_map_total_n()`]. It creates new, "factory-made" functions
#'   for consistency tests such as GRIM or GRIMMER. The new functions take
#'   reported summary statistics (e.g., means) and apply those tests in cases
#'   where only a total sample size is known, not group sizes.
#'
#'   This works by making [`disperse_total()`] create multiple pairs of
#'   hypothetical group sizes, all of which add up to the reported total. There
#'   need to be exactly two groups.
#'
#'   For background and more examples, see the
#'   \href{https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#total-n-mapper}{total-n
#'   mapper section} of *Consistency tests in depth*.
#'
#' @param .fun Function such as [`grim_map()`], or one made by
#'   [`function_map()`]: It will be used to test columns in a data frame for
#'   consistency. Test results are logical and need to be contained in a column
#'   called `consistency` that is added to the input data frame. This modified
#'   data frame is then returned by `.fun`.
#' @param .reported String. Names of the columns containing group-specific
#'   statistics that were reported alongside the total sample size(s). They will
#'   be tested for consistency with the hypothetical group sizes. Examples are
#'   `"x"` for GRIM and `c("x", "sd")` for DEBIT. In the data frame with
#'   reported group statistics that the manufactured function takes as an input,
#'   each will need to fan out like `"x1"`, `"x2"`, `"sd1"`, and `"sd2"`.
#' @param .name_test String (length 1). The name of the consistency test, such
#'   as `"GRIM"`, to be optionally shown in a message when using the
#'   manufactured function.
#' @param .name_class String. If specified, the tibbles returned by the
#'   manufactured function will inherit this string as an S3 class. Default is
#'   `NULL`, i.e., no extra class.
#' @param .dispersion,.n_min,.n_max,.constant,.constant_index Arguments passed
#'   down to [`disperse_total()`], using defaults from there.
#' @param ... These dots must be empty.
#'
#' @inheritParams function_map
#'
#' @details If functions created by `function_map_total_n()` are exported from
#'   other packages, they should be written as if they were created with
#'   \href{https://purrr.tidyverse.org/reference/faq-adverbs-export.html}{purrr
#'   adverbs}; see explanations there, and examples in the
#'   \href{https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#context-and-export}{export
#'   section} of *Consistency tests in depth*.
#'
#'   This function is a so-called function factory: It produces other functions,
#'   such as [`grim_map_total_n()`]. More specifically, it is a function
#'   operator because it also takes functions as inputs, such as [`grim_map()`].
#'   See Wickham (2019), ch. 10-11.
#'
#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://journals.sagepub.com/doi/10.1177/09567976211058727
#'
#'   Wickham, H. (2019). *Advanced R* (Second Edition). CRC Press/Taylor and
#'   Francis Group. https://adv-r.hadley.nz/index.html

#' @return A function such as these:
#'
#'   | \strong{Manufactured function}   | \strong{Reported statistics}  | \strong{Test vignette}
#'   | ---                              | ---                           | ---
#'   | [`grim_map_total_n()`]           | `"x"`                         | `vignette("grim")`
#'   | [`grimmer_map_total_n()`]        | `"x"`, `"sd"`                 | `vignette("grimmer")`
#'   | [`debit_map_total_n()`]          | `"x"`, `"sd"`                 | `vignette("debit")`
#'
#'   The factory-made function will also have dots, `...`, to pass arguments
#'   down to `.fun`, i.e., the basic mapper function.

#' @section Conventions: The name of a function returned by
#'   `function_map_total_n()` should mechanically follow from that of
#'   the input function. For example, [`grim_map_total_n()`] derives
#'   from [`grim_map()`]. This pattern fits best if the input function itself
#'   is named after the test it performs on a data frame, followed by `_map`:
#'   [`grim_map()`] applies GRIM, [`grimmer_map()`] applies GRIMMER, etc.
#'
#'   Much the same is true for the classes of data frames returned by the
#'   manufactured function via the `.name_class` argument of
#'   `function_map_total_n()`. It should be the function's own name preceded
#'   by the name of the package that contains it, or by an acronym of that
#'   package's name. Therefore, some existing classes are
#'   `scr_grim_map_total_n` and `scr_grimmer_map_total_n`.

#' @seealso [`function_map_seq()`]
#'
#' @include utils.R disperse.R function-factory-helpers.R
#'
#' @export
#'
#' @examples
#' # Function definition of `grim_map_total_n()`:
#' grim_map_total_n <- function_map_total_n(
#'   .fun = grim_map,
#'   .reported = "x",
#'   .name_test = "GRIM"
#' )

# TODO: ADD LINKS IN THE DOCS VIA SQUARE BRACKETS! GO THROUGH THE FILES
# ALPHABETICALLY, STARTING WITH: function-map.R ALSO CHANGE
# `write_doc_factory_map_conventions()` TO INCLUDE BRACKETS IN THE OUTPUT, THEN
# RE-INSERT THE OUTPUT INTO THE DOCS OF THE THREE FACTORIES!

# # Full example inputs:
#
# data <- tibble::tribble(
#   ~x1,    ~ x2 ,  ~n,
#   "3.43", "5.28", 90,
#   "2.97", "4.42", 103,
#   "0.54", "0.81", 76
# )
#
# fun <- grim_map
# reported <- "x"
# name_test <- "GRIM"
# dispersion <- 0:5
# n_min <- 1
# n_max <- NULL


function_map_total_n <- function(.fun, .reported, .name_test,
                                 .name_key_result = "consistency",
                                 .name_class = NULL,
                                 .dispersion = 0:5,
                                 .n_min = 1L, .n_max = NULL,
                                 .constant = NULL,
                                 .constant_index = NULL, ...) {

  force(.fun)
  force(.reported)
  force(.name_test)
  force(.name_key_result)
  force(.name_class)
  force(.dispersion)
  force(.n_min)
  force(.n_max)
  force(.constant)
  force(.constant_index)

  # The dots are only included to prevent a false-positive CRAN warning, so they
  # must not be used:
  rlang::check_dots_empty()

  # Throw error if `n` itself was named as a reported statistic:
  if (any("n" == .reported)) {
    cli::cli_abort(c(
      "Can't take \"n\" as a reported statistic.",
      "i" = "Functions produced by `function_map_total_n()` \\
      assume that group-wise `n` values were not reported, \\
      and deal with them in hypothetical terms.",
      "i" = "Therefore, \"n\" can't be a `.reported` value."
    ))
  }

  name_fun <- deparse(substitute(.fun))

  reported_reduplicated <- rep(.reported, each = 2L)
  reported_reduplicated <- paste0(reported_reduplicated, c("1", "2"))


  # --- Start of the manufactured function, `fn_out()` ---

  fn_out <- rlang::new_function(
    args = rlang::pairlist2(
      data = ,
      dispersion = .dispersion,
      n_min = .n_min,
      n_max = .n_max,
      constant = .constant,
      constant_index = .constant_index,
      ... =
    ),
    body = rlang::expr({

      name_test <- `!!`(.name_test)
      name_fun <- `!!`(name_fun)
      name_class <- `!!`(.name_class)
      reported <- `!!`(.reported)
      reported_reduplicated <- `!!`(reported_reduplicated)
      fun <- `!!`(.fun)

      data <- absorb_key_args(data, reported_reduplicated)


      # Checks ---

      check_factory_dots(fun, name_fun, ...)

      # The usual key argument check conducted by
      # `check_mapper_input_colnames()` is not applicable to `data`, so the
      # function only checks the remaining point, using an internal helper:
      check_consistency_not_in_colnames(data, name_test)

      check_tibble(data)

      # Make sure that the `n` column is present...
      if (!any(colnames(data) == "n")) {
        cli::cli_abort(c(
          "Column `n` missing.",
          "i" = "`n` should contain the reported total sample sizes \\
          (one per row).",
          "i" = "The function will use `disperse_total()` to go up \\
          and down from the integer at half an even `n`, or the \\
          two integers just around half an odd `n`."
        ))
      }

      # ...and that all of its values are whole numbers:
      offenders <- data$n[!is_whole_number(data$n)]

      if (length(offenders) > 0L) {
        if (length(offenders) > 3L) {
          offenders <- offenders[1:3]
          msg_starting_with <- ", starting with"
        } else {
          msg_starting_with <- ":"
        }
        msg_offenders <- wrap_in_backticks(offenders)
        cli::cli_abort(c(
          "`n` values must be whole numbers.",
          "x" = "The `n` column includes decimal \\
          numbers{msg_starting_with} {msg_offenders}.",
          "i" = "They are supposed to be (total) sample sizes."
        ))
      }


      # Main part ---

      data_forth <- data
      data_back  <- data

      reported_orig <- reported

      # Determine the column names with `"1"` or `"2"` that should be in `data`,
      # going by `reported` (specified in the call to the function factory):
      cols_expected_forth <- reported_reduplicated

      # Since all of these reported columns matter to a test with dispersed
      # group sizes, check if any of them are missing from `data`...
      cols_missing <-
        cols_expected_forth[!cols_expected_forth %in% colnames(data)]

      # ...and if so, throw an error:
      if (length(cols_missing) > 0L) {
        if (length(cols_missing) == 1L) {
          msg_cols <- "Column"
          msg_is_are <- "is"
          msg_it_they <- "It's"
          msg_this_these <- "This column"
        } else {
          msg_cols <- "Columns"
          msg_is_are <- "are"
          msg_it_they <- "They're"
          msg_this_these <- "These columns"
        }
        msg_cols_missing <- wrap_in_backticks(cols_missing)
        cli::cli_abort(c(
          "{msg_cols} {msg_cols_missing} {msg_is_are} missing from `data`.",
          "i" = "{msg_it_they} expected because of the `.reported` \\
          specification in the call to `function_map_total_n()` that \\
          created the present function.",
          "i" = "{msg_this_these} should contain reported group statistics. \\
          {msg_it_they} presumably essential to {name_test}."
        ))
      }

      # Switch `"1"` and `"2"` in the relevant column names of `data`:
      temp <- "_scr_names_temp_placeholder"
      cols_expected_back <- cols_expected_forth %>%
        stringr::str_replace("1", temp) %>%
        stringr::str_replace("2", "1") %>%
        stringr::str_replace(temp, "2")

      # Bring the names with switched index portions back into the `data_back`
      # tibble (because all of this switching is only for `data_back`):
      names(data_back)[names(data_back) %in% cols_expected_back] <-
        cols_expected_back

      # Needed below for ordering:
      cols_forth_order <- cols_expected_forth

      # Complete the switching by ordering the relevant columns so that the
      # column names are as in the original `data` (and hence, as in
      # `data_forth`), but the columns themselves -- the values -- have switched
      # positions. This goes by `cols_forth_order` because that's what will lead
      # to column names identical to those in `data_forth`:
      data_back <- data_back %>%
        dplyr::relocate(all_of(cols_forth_order))

      # Isolate the expected columns:
      cols_expected_forth <- data_forth %>%
        dplyr::select(all_of(cols_expected_forth))

      cols_expected_back <- data_back %>%
        dplyr::select(all_of(cols_expected_back)) %>%
        dplyr::relocate(all_of(cols_forth_order))

      # Generate the lower-level "proto" function that will apply
      # `disperse_total` and `fun` (the test-specific mapping function, such as
      # `grim_map`) to `data_forth` and `data_back`:
      map_total_n_proto <- function_map_total_n_proto(
        .fun = fun, .reported = cols_expected_forth,
        .reported_orig = reported_orig, .dispersion = dispersion,
        .n_min = n_min, .n_max = n_max,
        .constant = constant,
        ...
      )

      # Now, call the manufactured function on both tibbles. First the
      # original...
      out_forth <- map_total_n_proto(
        data = data_forth, reported = cols_expected_forth,
        reported_orig = reported_orig, dir = factor("forth", levels = "forth"),
        dispersion = dispersion,
        n_min = n_min, n_max = n_max,
        constant = constant,
        ...
      )

      # ...and second, the one with reversed index name portions:
      out_back  <- map_total_n_proto(
        data = data_back, reported = cols_expected_back,
        reported_orig = reported_orig, dir = factor("back", levels = "back"),
        dispersion = dispersion,
        n_min = n_min, n_max = n_max,
        constant = constant,
        ...
      )

      # In case of an internal error with these functions themselves due to
      # inconsistent results between the two `function_map_total_n_proto()`
      # calls right above, the user would be left completely in the dark. This
      # error message, then, would at least clarify the source of the problem:
      if (!all(colnames(out_forth) == colnames(out_back))) {
        names_out_forth <- wrap_in_backticks(colnames(out_forth))
        names_out_back  <- wrap_in_backticks(colnames(out_back))
        cli::cli_abort(c(
          "Column names returned by calls to the helper function \\
          `scrutiny:::function_map_total_n_proto()` are not identical.",
          "i" = "Column names in question --",
          "*" = "`colnames(out_forth)`: {names_out_forth}",
          "*" = "`colnames(out_back)`: {names_out_back}",
          "x" = "This is a deep error within at least one of the \\
          function operators `scrutiny::function_map_total_n()` \\
          and `scrutiny:::function_map_total_n_proto()`.",
          "i" = "Please check the source code for these."
        ))
      }

      # Combine the two sets of results into one final tibble:
      out <- dplyr::bind_rows(out_forth, out_back)
      out <- add_class(out, "scr_map_total_n")

      # This is a hack, but it works. Its solves the following problem:
      # `constant_index` is meant to work within the `disperse_total()` tibble,
      # but the final output tibble, `out`, looks very different from that
      # tibble. If `constant_index` were to operate on the dispersion tibble, as
      # in the `disperse()` functions themselves, there would be a mismatch
      # between the functionality of `constant_index` and the user-facing
      # tibble. Therefore, `constant_index` must play itself out here, not
      # within `disperse_total()`:
      if (!is.null(constant) && !is.null(constant_index)) {

        check_length(constant_index, 1L)

        if (is.null(names(constant))) {
          constant_ref <- "constant"
        } else {
          constant_ref <- names(constant)
        }

        out <- dplyr::relocate(
          out, all_of(constant_ref), .before = constant_index
        )
      }

      # If `.name_class` (note the dot) was specified in the course of creating
      # the manufactured function, its value will become a class of the output:
      if (!is.null(name_class)) {
        out <- add_class(out, name_class)
      }

      `!!!`(write_code_col_key_result(.name_key_result))
    }),
    env = rlang::caller_env()
  )


  # --- End of the manufactured function, `fn_out()` ---

  # Garbage collection:
  rm(name_fun)

  # Duplicate the names of the statistics reported pairwise with one total `n`
  # per pair. Paste `"1"` and `"2"` at the ends of these names, then add them to
  # the list of arguments of the manufactured function:
  insert_key_args(fn_out, reported_reduplicated)
}
