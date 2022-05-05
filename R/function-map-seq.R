

#' @include grim-map.R

function_map_seq <- function(.fun, .name_test, .name_class = NULL,
                             .dispersion = 0:5, ...) {

  # The function factory returns this manufactured function:
  function(fun = .fun, name_test = .name_test, name_class = .name_class,
           dispersion = .dispersion, ...) {
    # MORE HERE! IN PARTICULAR, DECIDE BETWEEN `seq_distance()` AND
    # `seq_endpoint()`.
  }

  # -- End of the manufactured function --

}



# Playground --------------------------------------------------------------

# Helper used within the `*_proto()` function below; not exported:
seq_disperse_df <- function(x, dispersion, include_reported = TRUE) {

  digits <- max(decimal_places(x))
  p10 <- 1 / (10 ^ digits)
  dispersion <- dispersion * p10

  x_num <- as.numeric(x)

  x_minus <- x_num - dispersion
  x_plus  <- x_num + dispersion

  if (include_reported) {
    out <- append(rev(x_minus), c(x_num, x_plus))
  } else {
    out <- append(rev(x_minus), x_plus)
  }

  if (is.character(x)) {
    out <- restore_zeros(out, width = digits)
  }

  out <- tibble::tibble(out)

  return(out)
}


# # Example data:
# data <- grim_map(pigs1)
# fun  <- grim_map
# var  <- "n"    # `var` is the variable to be varied, i.e., dispersed.
# dispersion <- 1:5
# include_reported <- TRUE


function_map_seq_proto <- function(.fun = fun, .var = var,
                                   .dispersion = dispersion,
                                   .include_reported = include_reported, ...) {

  # The function factory returns this manufactured function:
  function(data, fun = .fun, var = .var, dispersion = .dispersion,
           include_reported = .include_reported, ...) {

    data_var <- match(var, colnames(data))
    data_var <- data[data_var]
    data_var <- data_var[[1]]

    list_var <- purrr::map(
      data_var,
      seq_disperse_df,
      dispersion = dispersion,
      include_reported = include_reported
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
      dplyr::across({{ cols_el }}, as.list),
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


x <- 0.25
dispersion <- 0:5



# Manufactured functions --------------------------------------------------

grim_map_seq <- function_map_seq(
  .fun = grim_map,
  .name_test = "GRIM",
  .name_class = "scr_grim_map_seq"
)
