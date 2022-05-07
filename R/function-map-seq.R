

#' @include grim-map.R


# Playground --------------------------------------------------------------

# # Helper used within the `*_proto()` function below; not exported:
# seq_disperse_df_custom <- function(x, dispersion, include_reported = FALSE) {
#
#   digits <- max(decimal_places(x))
#   p10 <- 1 / (10 ^ digits)
#   dispersion <- dispersion * p10
#
#   x_num <- as.numeric(x)
#
#   x_minus <- x_num - dispersion
#   x_plus  <- x_num + dispersion
#
#   if (include_reported) {
#     out <- append(rev(x_minus), c(x_num, x_plus))
#   } else {
#     out <- append(rev(x_minus), x_plus)
#   }
#
#   if (is.character(x)) {
#     out <- restore_zeros(out, width = digits)
#   }
#
#   out <- tibble::tibble(out)
#
#   return(out)
# }




# Function factories ------------------------------------------------------

# Note on helpers and implementation: Unlike `function_map_total_n()`, the main
# function here -- `function_map_seq()` -- is not based on `disperse()` or its
# derivatives. It is not based on `seq_endpoint()` or friends either, which is
# surprising but necessary: `disperse()` is pair-based and does not construct a
# linear sequence, whereas `seq_endpoint()` and friends lack support for
# dispersion and would have been very cumbersome with regard to the
# `include_reported` argument. It became clear that I needed something new -- a
# blend of both aspects. I wrote `seq_disperse()` and `seq_disperse_df()`, and I
# applied the latter within the internal helper function factory below,
# `function_map_seq_proto()`.


function_map_seq_proto <- function(.fun = fun, .var = var,
                                   .dispersion = dispersion,
                                   .include_reported = include_reported, ...) {

  # The function factory returns this manufactured function:
  function(data, fun = .fun, var = .var, dispersion = .dispersion,
           include_reported = .include_reported, ...) {

    # Extract the vector from the `data` column specified as `var`:
    data_var <- data[var][[1]]

    # list_var <- purrr::map(
    #   data_var,
    #   seq_disperse_df_custom,
    #   dispersion = dispersion,
    #   include_reported = include_reported
    # )

    list_var <- purrr::map(
      data_var,
      seq_disperse_df,
      .dispersion = dispersion,
      .string_output = "auto",
      .include_reported = include_reported
    )

    nrow_list_var <- purrr::map_int(list_var, nrow)

    ncol_index_var <- match(var, colnames(data))
    ncol_before_consistency <- match("consistency", colnames(data)) - 1

    cols_for_testing <- data[, 1:ncol_before_consistency]
    cols_for_testing_names_without_var <-
      colnames(cols_for_testing)[!colnames(cols_for_testing) == var]

    # Short for "columns except (for the) last (one)":
    cols_el <- 1:length(cols_for_testing_names_without_var)

    data_list_without_var <- dplyr::mutate(
      data[cols_for_testing_names_without_var],
      nrow_list_var,
      dplyr::across({{ cols_el }}, ~ purrr::map2(., nrow_list_var, rep)),
      nrow_list_var = NULL
    )

    # Prepare the data frames for testing:
    data_list_for_testing <- data_list_without_var %>%
      dplyr::mutate(
        {{ var }} := purrr::map(list_var, dplyr::pull),
        .before = all_of(ncol_index_var)
      ) %>%
      split_into_rows() %>%
      purrr::map(
        tibble::as_tibble,
        .name_repair = ~ colnames(cols_for_testing)
      )

    # Apply the testing function, `fun`, to all data frames in the list:
    data_list_tested <- data_list_for_testing %>%
      purrr::map(fun, ...)

    # Mark the original case (i.e., row in `data`, the input data frame):
    case <- data_list_tested %>%
      purrr::map_int(nrow) %>%
      purrr::map2(1:length(data_list_tested), ., rep) %>%
      purrr::flatten_int()

    # Combine all output data frames to one. As each of them represents one row
    # of the input data frame -- one "case" -- distinguish them by `case`:
    out <- data_list_tested %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(case)

    return(out)
  }

  # -- End of the manufactured function --

}



# # Example data:
# x <- 0.25
# dispersion <- 1:5
#
# data <- grim_map(pigs1)
# fun  <- grim_map
# var  <- c("x", "n")    # `var` is the variable to be varied, i.e., dispersed.
# dispersion <- 1:5
# include_reported <- TRUE
#
# .fun <- grim_map
# .name_test <- "GRIM"
# .name_class <- "scr_grim_map_seq"
# .dispersion <- 1:5
#
# fun        <- .fun
# name_test  <- .name_test
# name_class <- .name_class
# dispersion <- .dispersion




#' Create new `*_map_seq()` functions
#'
#' @description `function_map_seq()` is the engine that powers `grim_map_seq()`
#'   and `debit_map_seq()`. It creates new, "manufactured" functions that apply
#'   consistency tests such as GRIM or DEBIT to sequences of specified
#'   variables. The sequences are centered around the reported values of those
#'   variables.
#'
#'   By default, only inconsistent values are dispersed from and tested. This
#'   provides an easy and powerful way to assess whether small errors in
#'   computing or reporting may be responsible for previously determined
#'   inconsistencies.
#'
#'   If functions created this way are exported from other packages, they should
#'   be written as if they were created with
#'   \href{https://purrr.tidyverse.org/reference/faq-adverbs-export.html}{purrr
#'   adverbs}; see explanations there and examples below.
#'
#' @param .fun Function such as `grim_map()`: It will be used to test columns in
#'   a data frame for consistency. Test results are Boolean and need to be
#'   contained in a column called `"consistency"` that is added to the input
#'   data frame. This modified data frame is then returned by `.fun`.
#' @param .name_test String (length 1). The name of the consistency test, such
#'   as `"GRIM"`, to be optionally shown in a message when using the
#'   manufactured function.
#' @param .name_class String. If specified, the tibbles returned by the
#'   manufactured function will inherit this string as an S3 class. Default is
#'   `NULL`, i.e., no extra class.
#' @param .dispersion Numeric. Sequence with steps up and down from the reported
#'   values. It will be adjusted to these values' decimal level. For example,
#'   with a reported `8.34`, the step size is `0.01`. Default is `1:5`, for five
#'   steps up and down.
#' @param .include_reported Boolean. Should the reported values themselves be
#'   included in the sequences originating from them? Default is `FALSE` because
#'   this might be redundant and bias the results.
#' @param .include_consistent Boolean. Should the function also process
#'   consistent cases (from among those reported), not just inconsistent ones?
#'   Default is `FALSE` because the focus should be on clarifying
#'   inconsistencies.
#' @param ... Arguments passed down to `.fun`.
#'
#' @return
#' @export

# @examples


function_map_seq <- function(.fun, .name_test, .name_class = NULL,
                             .dispersion = 1:5, .include_reported = FALSE,
                             .include_consistent = FALSE, ...) {

  # The function factory returns this manufactured function:
  function(data, var, fun = .fun, name_test = .name_test,
           name_class = .name_class, dispersion = .dispersion,
           include_reported = .include_reported,
           include_consistent = .include_consistent, ...) {

    # Remove consistent cases from `data` if only the inconsistent ones are of
    # interest (the default). The "filtering" code below is equivalent to
    # `dplyr::filter(data, !consistency)`, but much faster.
    if (!include_consistent) {
      data <- data[!data$consistency, ]
    }

    # ncol_index_consistency        <- match("consistency", colnames(data))
    # ncol_index_before_consistency <- 1:(ncol_index_consistency - 1)
    # ncol_index_after_consistency  <- (ncol_index_consistency + 1):ncol(data)
    #
    # data_before_consistency <- data[ncol_index_before_consistency]
    # data_after_consistency  <- data[ncol_index_after_consistency]


    # Create the lower-level testing function via an internal function factory:
    map_seq_proto <- function_map_seq_proto(
      .fun = fun,
      .name_test = name_test,
      .name_class = name_class,
      .dispersion = dispersion,
      .include_reported = include_reported
    )

    # Apply the lower-level function to all user-supplied variables (`var`) and
    # all cases reported in `data`, or at least the inconsistent ones:
    out <- purrr::map(var, ~ map_seq_proto(data = data, var = .x))

    nrow_out <- purrr::map_int(out, nrow)

    # Repeat the `var` strings so that they will be able to be added to `out`:
    var <- var %>%
      purrr::map2(nrow_out, rep) %>%
      purrr::flatten_chr()

    # For better output, `out` should be a single data frame; and for
    # identifying the origin of individual rows, `var` is added:
    out <- out %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(var)

    if (!is.null(name_class)) {
      out <- add_class(out, name_class)
    }

    return(out)
  }

  # -- End of the manufactured function --

}




# Manufactured functions --------------------------------------------------

grim_map_seq <- function_map_seq(
  .fun = grim_map,
  .name_test = "GRIM",
  .name_class = "scr_grim_map_seq"
)

