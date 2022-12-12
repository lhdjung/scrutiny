
# Helper function used within `function_map_total_n_proto()` below; not
# exported:
mutate_both_consistent <- function(data) {
  both_consistent <- data$consistency %>%
    split_into_groups(group_size = 2) %>%
    purrr::map_lgl(all) %>%
    rep(each = 2)

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
                                       .n_min = 1, .n_max = NULL,
                                       .constant = NULL, ...) {

  function(data, fun = .fun, reported = .reported,
           reported_orig = .reported_orig, dir = .dir,
           dispersion = .dispersion,
           n_min = .n_min, n_max = .n_max,
           constant = .constant, ...) {

    reported_names  <- colnames(reported)
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
    df_list_nrow <- purrr::map_int(df_list, nrow)
    df_list_n_groups <- length(df_list_nrow)

    out_df_nested <- reported %>%
      split_into_rows() %>%
      purrr::map(split_into_groups, group_size = 2) %>%
      tibble::tibble(.name_repair = ~ "reported") %>%
      dplyr::mutate(
        df_list,
        times    = df_list_nrow / 2,
        reported = purrr::map2(reported, times, ~ purrr::map2(.x, .y, rep))
      ) %>%
      tidyr::unnest_wider(reported) %>%
      dplyr::rename_with(
        .fn   = ~ reported_orig,
        .cols = 1:dplyr::all_of(reported_n_vars)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          1:dplyr::all_of(reported_n_vars),
          ~ purrr::map(., tibble::as_tibble)
        ), times = NULL
      )

    # Rename the tibbles nested within the list-columns using the original names
    # of the reported variables (e.g., `c("x", "sd")`) so that it's clear what
    # those values represent:
    out_df_nested[1:reported_n_vars] <-
      out_df_nested[1:reported_n_vars] %>%
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
      purrr::map2(1:df_list_n_groups, ., rep) %>%
      purrr::flatten_int()

    out_df <- tidyr::unnest(out_df_nested, cols = everything())
    n_change <- out_df$n_change
    colnames(out_df)[1:length(reported_orig)] <- reported_orig

    out_df <- fun(out_df, ...)

    if (!"n_change" %in% colnames(out_df)) {
      out_df <- dplyr::mutate(out_df, n_change)
    }

    out_df <- out_df %>%
      mutate_both_consistent() %>%
      dplyr::mutate(case, dir) %>%
      dplyr::relocate(n_change, .after = n)

    return(out_df)
  }

  # -- End of the manufactured function --

}




#' Create new `*_map_total_n()` functions
#'
#' @description `function_map_total_n()` is the engine that powers functions
#'   such as `grim_map_total_n()`. It creates new, "manufactured" functions for
#'   consistency tests. The new functions take reported summary statistics such
#'   as means and apply those tests in cases where only a total sample size is
#'   known, not group sizes.
#'
#'   This works by making `disperse_total()` create multiple pairs of
#'   hypothetical group sizes, all of which add up to the reported total. There
#'   need to be exactly two groups.
#'
#'   If functions created this way are exported from other packages, they should
#'   be written as if they were created with
#'   \href{https://purrr.tidyverse.org/reference/faq-adverbs-export.html}{purrr
#'   adverbs}; see explanations there and examples at
#'   `vignette("consistency-tests")`, section *Creating mappers with
#'   `function_map()`*.
#'
#' @param .fun Function such as `grim_map`: It will be used to test columns in a
#'   data frame for consistency. Test results are Boolean and need to be
#'   contained in a column called `consistency` that is added to the input data
#'   frame. This modified data frame is then returned by `.fun`.
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
#'   down to `disperse_total()`, using defaults from there.
#'
#' @details This function is a so-called function factory: It produces other
#'   functions, such as `grim_map_total_n()`. More specifically, it is a
#'   function operator (a.k.a. decorator) because it also takes functions as
#'   inputs, such as `grim_map()`. See Wickham (2019, ch. 10-11).
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
#'   | \strong{Manufactured function} | \strong{Reported statistics}  | \strong{Test vignette}
#'   | ---                            | ---                           | ---
#'   | `grim_map_total_n()`           | `"x"`                         | `vignette("grim")`
#'   | `grimmer_map_total_n()`        | `"x"`, `"sd"`                 | `vignette("grimmer")`
#'   | `debit_map_total_n()`          | `"x"`, `"sd"`                 | `vignette("debit")`
#'
#'   The factory-made function will also have dots, `...`, to pass arguments
#'   down to `.fun`, i.e., the basic mapper function.

#' @section Conventions: The name of a function manufactured with
#'   `function_map_total_n()` should mechanically follow from that of the input
#'   function. For example, `grim_map_total_n()` derives from `grim_map()`.
#'   This pattern fits best if the input function itself is named after the test
#'   it performs on a data frame, followed by `_map`: `grim_map()` applies GRIM,
#'   `debit_map()` applies DEBIT, etc.
#'
#'   Much the same is true for the classes of data frames returned by the
#'   manufactured function via the `.name_class` argument of
#'   `function_map_total_n()`. It should be the function's own name preceded by
#'   the name of the package that contains it or by an acronym of that package's
#'   name. In this way, existing classes are `scr_grim_map_total_n` and
#'   `scr_debit_map_total_n`.

#' @seealso `disperse_total()`
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
#'   .name_test = "GRIM",
#'   .name_class = "scr_grim_map_total_n"
#' )
#'
#'
#' # Case study of SCHLIM, a new consistency test --------------
#'
#' # (Note: This is a mock test without any real significance.
#' # Its only purpose is to show the minimal steps necessary
#' # for implementing a serious consistency test, and to use
#' # it as a starting point for `function_map_total_n()`.)
#'
#' # The "SCHLIM test" is analogous to GRIM as implemented
#' # in scrutiny. This is also true for the function names.
#' # Note that the analogue to `schlim_scalar()`, a function
#' # called `grim_scalar()`, is not exported from scrutiny,
#' # but used internally for `grim()`, `grim_map()`, and,
#' # indirectly, `grim_map_total_n()`.
#'
#' # Basic test implementation:
#' schlim_scalar <- function(y, n) {
#'   (y / 3) > n
#' }
#'
#' # This step is not needed below, but
#' # included for completeness:
#' schlim <- Vectorize(schlim_scalar)
#'
#' # This will be the input function for
#' # `function_map_total_n()`:
#' schlim_map <- function_map(
#'   .fun = schlim_scalar,
#'   .reported = c("y", "n"),
#'   .name_test = "SCHLIM"
#' )
#'
#' # Fire up the function factory:
#' schlim_map_total_n <- function_map_total_n(
#'   .fun = schlim_map,
#'   .reported = "y",
#'   .name_test = "SCHLIM",
#' )
#'
#' # Create some example data:
#' df1 <- tibble::tibble(
#'   y1 = 16:25,
#'   y2 = 26:35,
#'   n  = 12:21
#' )
#' df1
#'
#' # Call the manufactured function:
#' out <- schlim_map_total_n(df1)
#' out
#'
#' # Summarize the results:
#' audit_total_n(out)




# # Example specifications:
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
                                 .name_class = NULL,
                                 .dispersion = 0:5,
                                 .n_min = 1, .n_max = NULL,
                                 .constant = NULL,
                                 .constant_index = NULL) {

  force(.fun)
  force(.reported)
  force(.name_test)
  force(.name_class)
  force(.dispersion)
  force(.n_min)
  force(.n_max)
  force(.constant)
  force(.constant_index)

  # Throw error if `n` itself was named as a reported statistic:
  if ("n" %in% .reported) {
    cli::cli_abort(c(
      "Don't specify \"n\" as a reported statistic.",
      "x" = "Functions produced by `function_map_total_n()` \\
      assume that group-wise `n` values were not reported, \\
      and deal with them in hypothetical terms.",
      "x" = "Therefore, it can't be a `.reported` value."
    ))
  }

  fun_name <- deparse(substitute(.fun))

  reported_reduplicated <- rep(.reported, each = 2)
  reported_reduplicated <- paste0(reported_reduplicated, c("1", "2"))


  # --- Start of the manufactured function, `fn_out()` ---

  fn_out <- function(data, dispersion = .dispersion,
                     n_min = .n_min, n_max = .n_max,
                     constant = .constant,
                     constant_index = .constant_index, ...) {

    fun <- .fun
    reported <- .reported
    name_test <- .name_test
    name_class <- .name_class

    data <- absorb_key_args(data, reported_reduplicated)


    # Checks ---

    check_factory_dots(fun, fun_name, ...)

    # The usual key argument check conducted by `check_mapper_input_colnames()`
    # is not applicable to `data`, so the function only checks the remaining
    # point, using an internal helper:
    check_consistency_not_in_colnames(data, name_test)

    # Make sure that the `n` column is present...
    if (!"n" %in% colnames(data)) {
      cli::cli_abort(c(
        "Column `n` missing.",
        "x" = "`n` should contain the reported total sample sizes \\
        (one per row).",
        "i" = "The function will use `disperse_total()` to go up \\
        and down from the integer at half an even `n`, or the \\
        two integers just around half an odd `n`."
      ))
    }

    # ...and that all of its values are whole numbers:
    offenders <- data$n[!is_whole_number(data$n)]

    if (length(offenders) > 0) {
      if (length(offenders) > 3) {
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

    # Since all of these reported columns matter to a test with dispersed group
    # sizes, check if any of them are missing from `data`...
    cols_missing <-
      cols_expected_forth[!cols_expected_forth %in% colnames(data)]

    # ...and if so, throw an error:
    if (length(cols_missing) > 0) {
      if (length(cols_missing) == 1) {
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
        "x" = "{msg_it_they} expected because of the `.reported` \\
        specification in the call to `function_map_total_n()` that \\
        created the present function.",
        "x" = "{msg_this_these} should contain reported group statistics. \\
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

    # Complete the switching by ordering the relevant columns so that the column
    # names are as in the original `data` (and hence, as in `data_forth`), but
    # the columns themselves -- the values -- have switched positions. This goes
    # by `cols_forth_order` because that's what will lead to column names
    # identical to those in `data_forth`:
    data_back <- data_back %>%
      dplyr::relocate(all_of(cols_forth_order))

    # Isolate the expected columns:
    cols_expected_forth <- data_forth %>%
      dplyr::select(all_of(cols_expected_forth))

    cols_expected_back <- data_back %>%
      dplyr::select(all_of(cols_expected_back)) %>%
      dplyr::relocate(all_of(cols_forth_order))

    # Generate the lower-level "proto" function that will apply `disperse_total`
    # and `fun` (the test-specific mapping function, such as `grim_map`) to
    # `data_forth` and `data_back`:
    map_total_n_proto <- function_map_total_n_proto(
      .fun = fun, .reported = cols_expected_forth,
      .reported_orig = reported_orig, .dispersion = dispersion,
      .n_min = n_min, .n_max = n_max,
      .constant = constant,
      ...
    )

    # Now, call the manufactured function on both tibbles. First the original...
    out_forth <- map_total_n_proto(
      data = data_forth, reported = cols_expected_forth,
      reported_orig = reported_orig, dir = "forth",
      dispersion = dispersion,
      n_min = n_min, n_max = n_max,
      constant = constant,
      ...
    )

    # ...and second, the one with reversed index name portions:
    out_back  <- map_total_n_proto(
      data = data_back, reported = cols_expected_back,
      reported_orig = reported_orig, dir = "back",
      dispersion = dispersion,
      n_min = n_min, n_max = n_max,
      constant = constant,
      ...
    )

    # In case of an internal error with these functions themselves due to
    # inconsistent results between the two `function_map_total_n_proto()` calls
    # right above, the user would be left completely in the dark. This error
    # message, then, would at least clarify the source of the problem:
    if (!all(colnames(out_forth) == colnames(out_back))) {
      names_out_forth <- wrap_in_backticks(colnames(out_forth))
      names_out_back  <- wrap_in_backticks(colnames(out_back))
      cli::cli_abort(c(
        "Column names returned by calls to the helper function \\
        `scrutiny:::function_map_total_n_proto()` are not identical.",
        "i" = "Column names in question --",
        ">" = "`colnames(out_forth)`: {names_out_forth}",
        ">" = "`colnames(out_back)`: {names_out_back}",
        "x" = "This is a deep error within at least one of the \\
        function operators `scrutiny::function_map_total_n()` \\
        and `scrutiny:::function_map_total_n_proto()`.",
        "x" = "Please check the source code for these."
      ))
    }

    # Combine the two sets of results into one final tibble:
    out_total <- dplyr::bind_rows(out_forth, out_back)
    out_total <- add_class(out_total, "scr_map_total_n")

    # This is a hack, but it works. Its solves the following problem:
    # `constant_index` is meant to work within the `disperse_total()` tibble,
    # but the final output tibble, `out_total`, looks very different from that
    # tibble. If `constant_index` were to operate on the dispersion tibble, as
    # in the `disperse()` functions themselves, there would be a mismatch
    # between the functionality of `constant_index` and the user-facing tibble.
    # Therefore, `constant_index` needs to play itself out here, not within
    # `disperse_total()`:
    if (!is.null(constant) && !is.null(constant_index)) {

      check_length(constant_index, 1)

      if (is.null(names(constant))) {
        constant_ref <- "constant"
      } else {
        constant_ref <- names(constant)
      }

      out_total <- dplyr::relocate(
        out_total, all_of(constant_ref), .before = constant_index
      )
    }

    # If `.name_class` (note the dot) was specified in the course of creating
    # the manufactured function, its value will become a class of the output:
    if (!is.null(name_class)) {
      out_total <- add_class(out_total, name_class)
    }

    # Return the combined results:
    return(out_total)
  }

  # --- End of the manufactured function, `fn_out()` ---


  # Duplicate the names of the statistics reported pairwise with one total `n`
  # per pair. Paste `"1"` and `"2"` at the ends of these names, then add them to
  # the list of arguments of the manufactured function:
  fn_out <- insert_key_args(fn_out, reported_reduplicated)

  return(fn_out)
}
