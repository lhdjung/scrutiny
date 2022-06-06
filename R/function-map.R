

#' Create new `*_map()` functions
#'
#' @description `function_map()` creates new basic mapper functions such as
#'   `grim_map()` or `debit_map()`.
#'
#' @param .reported String. Names of the columns to be tested.
#' @param .fun Single-case function to be applied, such as the non-exported
#'   scrutiny functions `grim_scalar()` and `debit_scalar()`.
#' @param .name_class String. One or more classes to be added to the output data
#'   frame.
#'
#' @details If a `rounding` argument is specified via `...`, or else if `.fun`
#'   has a default for `rounding`, the output tibble inherits a class named
#'   `"scr_rounding_<rounding>"`.
#'
#'
#' @return A tibble (data frame) that also inherits `.name_class`.
#' @export

# data <- pigs1
# reported <- c("x", "n")
# fun <- grim_scalar
# name_test <- "GRIM"
# name_class <- "scr_grim_map"


function_map <- function(.reported, .fun, .name_class, ...) {

  # The factory returns this manufactured function:
  function(data, reported = .reported,
           fun = .fun, name_class = .name_class, ...) {

    dots <- rlang::enexprs(...)
    rounding_dots <- dots$rounding

    fun_args <- as.list(args(fun))
    rounding_args <- fun_args$rounding

    if (length(rounding_dots) > 0) {
      rounding <- rounding_dots
    } else if (length(rounding_args) > 0) {
      rounding <- rounding_args
    } else {
      rounding <- NULL
    }

    if (length(rounding) > 0) {
      rounding_class <- paste0("scr_rounding_", rounding)
      name_class <- append(name_class, rounding_class)
    }

    data_tested <- data[, reported]
    data_non_tested <- data[!colnames(data) %in% colnames(data_tested)]

    consistency <- purrr::pmap_lgl(data_tested, fun, ...)

    out <- dplyr::bind_cols(
      data_tested, consistency = consistency, data_non_tested
    )

    out <- add_class(out, name_class)

    return(out)
  }

  # --- End of the manufactured function ---

}

