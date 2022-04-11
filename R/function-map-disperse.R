
# Helper function used within `function_map_disperse_proto()` below; not
# exported:
mutate_both_consistent <- function(data) {
  consistency <- data$consistency
  both_consistent <- consistency %>%
    split(ceiling(seq_along(consistency) / 2)) %>%
    purrr::map_lgl(all) %>%
    rep(each = 2)

  dplyr::mutate(data, both_consistent, .after = "consistency")
}



# Used within `function_map_disperse()`:
function_map_disperse_proto <- function(.fun, .reported, .dispersion = 0:5,
                                        .n_min = 1, .n_max = NULL,
                                        .show_all = FALSE, ...) {

  function(data, fun = .fun, reported = .reported, dispersion = .dispersion,
           n_min = .n_min, n_max = .n_max,
           # x1 = NA, x2 = NA,
           show_all = .show_all, ...) {

    # df_count <- as.character(1:nrow(reported))

    # data <- data %>%
    #   dplyr::mutate(reported_index = 1:nrow(data))

    # reported_index <- 1:nrow(data)




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

    # # Example for `df_list`:
    # df_list <- list(
    #   disperse(45),
    #   disperse2(c(51, 52))[1:10, ],
    #   disperse(20)[1:8, ]
    # )



    reported_names <- names(reported)

    df_list <- data %>%
      dplyr::select(n) %>%
      purrr::pmap(
        disperse_total,
        # reported = reported, reported_index = reported_index,
        dispersion = dispersion,
        n_min = n_min, n_max = n_max
        # x1 = x1, x2 = x2
      )  # %>%
      # # NOTE: PROBABLY NEED TO WORK HARD TO GENERALIZE THE `c("x1", "x2")`
      # # PATTERN TO OTHER REPORTED STATISTICS; EITHER HERE OR ON THE LEVEL OF
      # # `disperse()`. -- HEY, I'VE GOT IT! IT'S THE `x1` AND `x2` ARGUMENTS TO
      # # `disperse()`! THAT'S THE PROBLEM! THEY ARE TAKEN BY `purrr::pmap()`...
      # purrr::map(fun, ...) %>%
      # purrr::map(mutate_both_consistent) %>%
      # purrr::map(dplyr::relocate, n_change, .after = both_consistent)


    n_reported_vars <- ncol(reported) / 2

    # Just as an example:
    df_list_nrow <- df_list %>%
      purrr::map_int(nrow)

    # In this very promising approach, `df_list_nrow` is the vector of numbers
    # of rows of the data frames constituting the list-elements of `df_list`;
    # where the latter doesn't take `reported` or `reported_index` into account
    # at all, and while only `disperse_total()` has been mapped onto it --
    # definitively not `fun` which, after all, is `grim_map` or `debit_map`! TO
    # DO: Find a way to name the `reported` column appropriately, given the
    # top-level input.
    out_df <- reported %>%
      split_into_rows() %>%
      purrr::map(split_into_n_groups, n = n_reported_vars) %>%
      tibble::tibble() %>%
      dplyr::rename(., reported = `.`) %>%
      dplyr::mutate(
        df_list,
        times    = df_list_nrow / 2,
        # reported = purrr::map(reported, purrr::flatten_chr),  # this needs to be split by `n_reported_vars`
        reported = purrr::map2(reported, times, ~ purrr::map2(.x, .y, rep)),
        # reported = purrr::map2(reported, times, ~ rep(.x, .y)),
        df_list  = purrr::map2(df_list, reported, ~ dplyr::mutate(.x, reported = .y))
        # reported = purrr::map2(reported, times, ~ purrr::map(.x, rep, .y)),
        # reported = purrr::map2(reported, times, rep),
        # reported = purrr::map2(reported, times, ~ .x[1:.y]),
        # reported = purrr::map(reported, purrr::as_vector),
        # df_list  = purrr::map2(df_list, reported, dplyr::mutate)
      )



    # reported %>%
    #   dplyr::mutate(df_list_nrow) %>%
    #   dplyr::mutate(dplyr::across(1:ncol(reported), as.list)) %>%
    #   dplyr::mutate(dplyr::across(1:ncol(reported), purrr::map, rep, times = 5))




    df_list_nrow <- df_list %>%
      purrr::map(nrow)  # %>%
      # purrr::map(
      #   tibble::tibble,
      #   scr_temp_nrow = rep(c("1", "2"), .[[i]] / 2)
      # )

    # reported %>%
    #   t() %>%
    #   tibble::as_tibble() %>%
    #   dplyr::rowwise() %>%
    #   as.list()




    # # Migrated from `disperse()` ----
    #
    #
    # # `reported` here is the same as `cols_expected` in the function factories!
    #   contains1 <- stringr::str_detect(names(reported), "1")
    #   reported_names_unique <- names(reported)[contains1] %>%
    #     stringr::str_remove("1")
    #
    #   reported_transposed <- reported %>%
    #     t() %>%
    #     tibble::as_tibble(.name_repair = ~ paste0("V", 1:nrow(reported)))
    #
    #   reported_transposed <- reported_transposed %>%
    #     .[rep(seq_len(nrow(.)), times = 3), ]
    #
    #   # reported_longer <- reported %>%
    #   #   tidyr::pivot_longer(
    #   #     cols = everything(),
    #   #     names_to = "term",
    #   #     values_to = "value"
    #   #   )
    #
    #   df_count <- reported %>%
    #     names() %>%
    #     as.integer()
    #
    #   # TO DO: Build a system that replaces the `1` constant in the
    #   # `dplyr::slice()` call by an integer variable that specifies the row number
    #   # appropriate for the present row of the tibble of reported summary data! It
    #   # will have to be passed down from the function factory level, probably from
    #   # the top level; via either an additional column (inelegant!) or an extra
    #   # class (maybe inefficient?):
    #   reported_vals <- reported %>%
    #     dplyr::slice(1) %>%
    #     purrr::as_vector()
    #
    #   # n_groups <- nrow(reported_vals) / 2
    #
    #   # The number of scenarios is half the number of dispersed `n` values:
    #   n_scenarios <- nrow(out) / 2
    #
    #   # `split_into_n_groups()` is an internal helper from the utils.R file. Its
    #   # second argument is the size of each resulting group, which has to be `2`
    #   # here because, for each variable in question, values for two groups were
    #   # reported:
    #   reported_groups <- reported_vals %>%
    #     split_into_n_groups(2) %>%
    #     purrr::map(rep, n_scenarios) %>%
    #     tibble::as_tibble()
    #
    #   # These groups correspond to the original names distilled from `reported`:
    #   names(reported_groups) <- reported_names_unique
    #
    #   # reported_df <- reported %>%
    #   #   purrr::map(rep, nrow(out) / 2) %>%
    #   #   tibble::as_tibble()
    #
    #   out <- out %>%
    #     dplyr::bind_cols(reported_groups, .)
    #
    #
    #
    #
    # # (End of migrated code)



    hits <- df_list %>%
      purrr::map(dplyr::filter, both_consistent) %>%
      purrr::map_int(nrow) %>%
      `/`(2) %>%
      as.integer()

    data <- data %>%
      dplyr::mutate(hits)

    return(list(data, df_list))
  }

}




#' Create new `*_map_disperse()` functions
#'
#' @description `function_map_disperse()` is the engine that powers
#'   `grim_map_disperse()`. It creates new, "manufactured" functions for
#'   consistency tests which take reported summary statistics such as means, to
#'   apply those tests in cases where only a total sample size is known, not
#'   group sizes.
#'
#'   If functions created this way are exported from other packages, they should
#'   be written as if they were created with
#'   \href{https://purrr.tidyverse.org/reference/faq-adverbs-export.html}{purrr
#'   adverbs}; see explanations there and examples below.
#'
#' @param .fun Function (length 1) such as `grim_map`: It will be used to test
#'   rows in a data frame for consistency.
#' @param .name_test String (length 1). The name of the consistency test, such
#'   as `"GRIM"`, to be optionally shown in a message when using the
#'   manufactured function.
#' @param .dispersion,.n_min,.n_max,.show_all Arguments passed down to
#'   `disperse_total()`, using defaults from there.
#' @param ... Arguments passed down to `.fun`.
#'
#' @details This function is a so-called function factory: It produces other
#'   functions, such as `grim_map_disperse()`. More specifically, it is a
#'   function operator (a.k.a. decorator) because it also takes functions as
#'   inputs, such as `grim_map()`. See Wickham (2019, ch. 10-11).
#'
#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://doi.org/10.1177/09567976211058727
#'
#'   Wickham, H. (2019). *Advanced R* (Second Edition). CRC Press/Taylor and
#'   Francis Group. https://adv-r.hadley.nz/index.html
#'
#' @return A function. See `grim_map_disperse()` for an example of a function
#'   manufactured by `function_map_disperse()`.
#'
#' @seealso `grim_map()`, `disperse_total()`, `grim_map_disperse()`
#'
#' @include utils.R disperse.R
#'
#' @export
#'
#' @examples
#' # Function definition of `grim_map_disperse()`:
#' grim_map_disperse <- function_map_disperse(
#'   .fun = grim_map, .name_test = "GRIM"
#' )
#'
#' # If you want to export a function manufactured
#' # with `function_map_disperse()` from your own
#' # package, follow this pattern (except for the
#' # `if`-wrapping here):
#' if (FALSE) {
#'
#'   schlim_map_disperse <- function(...) "dummy"
#'
#'   .onLoad <- function(lib, pkg) {
#'     schlim_map_disperse <<- scrutiny::function_map_disperse(
#'       .fun = schlim_map, .name_test = "SCHLIM"
#'     )
#'   }
#'
#' }




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
# show_all <- FALSE


function_map_disperse <- function(.fun, .reported, .name_test,
                                  .dispersion = 0:5,
                                  .n_min = 1, .n_max = NULL,
                                  .show_all = FALSE, ...) {


  # The function factory returns this manufactured function:
  function(data, fun = .fun, reported = .reported, name_test = .name_test,
           dispersion = .dispersion, n_min = .n_min, n_max = .n_max,
           show_all = .show_all, ...) {

    # Checks ---

    # offenders1 <- reported[!stringr::str_detect(reported, "1|2")]
    #
    # if (length(offenders1) > 0) {
    #   msg_offenders1 <- paste0("`", msg_offenders1, "`")
    #   if (length(msg_offenders1) < length(reported)) {
    #     msg_others <- ", among others"
    #   } else {
    #     msg_others <- ""
    #   }
    #   cli::cli_abort(c(
    #     "`reported` given as {msg_offenders1}{msg_others}.",
    #     "x" = "It needs to be pairs of columns that include \\
    #     \"1\" and \"2\", such as `reported = c(\"x1\", \"x2\")`."
    #   ))
    # }


    offenders2 <- data$n[!is_whole_number(data$n)]

    if (length(offenders2) > 0) {
      if (length(offenders2) > 3) {
        offenders2 <- offenders2[1:3]
        msg_starting_with <- ", starting with"
      } else {
        msg_starting_with <- ":"
      }
      msg_offenders2 <- paste0("`", offenders2, "`")
      cli::cli_abort(c(
        "`n` values must be whole numbers.",
        "x" = "The `n` column includes decimal \\
        numbers{msg_starting_with} {msg_offenders2}.",
        "i" = "They are supposed to be (total) sample sizes."
      ))
    }


    # Main part ---

    data_forth <- data
    data_back  <- data    # NEW!

    # data_back <- data %>%
    #   dplyr::mutate(
    #     temp = x2,
    #     x2 = x1,
    #     x1 = temp,
    #     temp = NULL
    #   )

    # # Isolate the names that contain `"1"` or `"2"`. This will also be used to
    # # switch the index portions (i.e., `"1"` and `"2"`) in the names of `data`:
    # cols_expected_back <- names(data)[stringr::str_detect(names(data), "1|2")]

    # cols_expected_back_missing <- cols_expected_back[!stringr::str_detect(cols_expected_back, reported)]
    #
    # if (length(cols_expected_back_missing) > 0) {}

    # Determine the column names with `"1"` or `"2"` that should be in `data`,
    # going by `reported` (specified in the call to the function operator):
    cols_expected_forth <- reported %>%
      rep(each = 2) %>%
      paste0(c("1", "2"))

    # Since all of these reported columns matter to a test with dispersed group
    # sizes, check if any of them are missing from `data`...
    cols_missing <- cols_expected_forth[!cols_expected_forth %in% colnames(data)]

    # ...and if so, throw an error:
    if (length(cols_missing) > 0) {
      if (length(cols_missing) == 1) {
        msg_cols <- "Column"
        msg_is_are <- "is"
        msg_it_they <- "It's"
        msg_this_these <- "This column is"
      } else {
        msg_cols <- "Columns"
        msg_is_are <- "are"
        msg_it_they <- "They're"
        msg_this_these <- "These columns are"
      }
      msg_cols_missing <- paste0("`", cols_missing, "`")
      cli::cli_abort(c(
        "{msg_cols} {msg_cols_missing} {msg_is_are} missing from `data`.",
        "x" = "{msg_it_they} expected because of the `.reported` \\
        specification in the call to `function_map_disperse()` that \\
        created the present function.",
        "x" = "{msg_this_these} presumably essential to {name_test}."
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
    # the columns itself -- the values -- have switched positions. This goes by
    # `cols_forth_order` because that's what leads to identical column names:
    data_back <- data_back %>%
      dplyr::relocate(all_of(cols_forth_order))


    # ...
    cols_expected_forth <- data_forth %>%
      dplyr::select(all_of(cols_expected_forth))

    cols_expected_back  <- data_back  %>%
      dplyr::select(all_of(cols_expected_back)) %>%
      dplyr::relocate(all_of(cols_forth_order))


    reports_enum <- 1:nrow(cols_expected_forth)



    # NOTE: The issue with only taking the first line of reported values is
    # somewhere within `map_disperse_proto()` or deeper!

    # Generate the lower-level "proto" function that will apply `disperse_total`
    # and `fun` (the test-specific mapping function, such as `grim_map`) to
    # `data_forth` and `data_back`:
    map_disperse_proto <- function_map_disperse_proto(
      .fun = fun, .reported = cols_expected_forth,
      .dispersion = dispersion,
      .n_min = n_min, .n_max = n_max, .show_all = show_all, ...
    )

    # Now, call the manufactured function on both tibbles. First the original...
    out_forth <- map_disperse_proto(
      data = data_forth, reported = cols_expected_forth,
      dispersion = dispersion, n_min = n_min, n_max = n_max,
      # x1 = x1, x2 = x2,
      ...
    )

    # ...and second, the one with reversed index name portions:
    out_back  <- map_disperse_proto(
      data = data_back, reported = cols_expected_back,
      dispersion = dispersion, n_min = n_min, n_max = n_max,
      # x1 = x1, x2 = x2,
      ...
    )

    # Isolate the number of both-consistent scenarios for each pairing...
    hits_forth <- out_forth[[1]]$hits
    hits_back  <- out_back[[1]]$hits

    # ...and sum them up:
    hits_total <- hits_forth + hits_back

    # As `out_forth` is structurally equivalent to `out_back`, counting
    # scenarios in the former is sufficient. Each tibble in `out_forth` has a
    # twin in `out_back`, so the number of rows needs to be doubled. However,
    # this is canceled out by halving the overall number (the latter step being
    # necessary because the number of scenarios is half the number of rows).
    # Therefore, the number of scenarios associated with each case is equal to
    # the number of rows in a single tibble representing that case:
    scenarios_total <- out_forth[[2]] %>%
      purrr::map_int(nrow)

    # Relate the (total) number of both-consistent scenarios to the total number
    # of scenarios:
    hit_rate <- hits_total / scenarios_total

    # Create an overall summary tibble with the input values and key results:
    out_summary <- data %>%
      dplyr::mutate(
        hits_forth, hits_back, hits_total, scenarios_total, hit_rate
      )


    if (show_all) {

      # Detailed results are prefixed with an explanation of all the tibbles:
      cli::cli_inform(c(
        "i" = "Explanation:",
        "*" = "Tibble [[1]] summarizes test results.",
        "*" = "Tibbles under [[2]] and [[3]] contain detailed analyses, \\
        with one tibble for each row in the input data frame.",
        "*" = "Those under [[2]] correspond to `hits_forth`, \\
        where `x2` is paired with the larger group.",
        "*" = "Those under [[3]] correspond to `hits_back`, \\
        where `x1` is paired with the larger group.",
        ">" = "(A \"hit\" is a scenario in which both dispersed `n` values \\
        are {name_test}-consistent with their corresponding `x*` values.)"
      ))

      # Remove the summary tibbles on top of each `map_disperse_proto()` output
      # list; they are no longer needed:
      out_forth <- out_forth[[-1]]
      out_back  <- out_back[[-1]]

      # Return results in a list with the overall summary tibble on top,
      # followed by the `out_forth` and `out_back` lists of tibbles with
      # detailed results:
      return(list(out_summary, out_forth, out_back))

    } else {

      # With the default `show_all = FALSE`, no lists with detailed results are
      # returned, only the overall summary tibble:
      return(out_summary)
    }
  }
}

