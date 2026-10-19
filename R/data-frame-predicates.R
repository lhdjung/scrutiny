#' Is an object the output of a mapper function?
#'
#' @description
#' - `is_map_df()` tests whether an object was returned by a scrutiny-style
#' mapper function of any tier: a basic mapper such as [`grim_map()`] or one
#' made by [`function_map()`], or a function made by [`function_map_seq()`] or
#' [`function_map_total_n()`].
#' - `is_map_basic_df()`, `is_map_seq_df()`, and `is_map_total_n_df()` test
#' for one tier each.
#'
#' @param x Object to be tested.
#'
#' @return Logical (length 1).
#'
#' @details See `vignette("consistency-tests-in-depth")` for the three tiers
#'   and the table at its end.
#'
#' @export
#'
#' @name data-frame-predicates
#'
#' @examples
#' df1 <- pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1)
#' df2 <- pigs1 |>
#'   grim_map_seq(digits_x = 2, items_averaged = 1)
#' df3 <- tibble::tribble(
#'   ~x1,  ~x2,  ~n,
#'   3.43, 5.28, 90,
#'   2.97, 4.42, 103
#' ) |>
#'   grim_map_total_n(digits_x = 2, items_averaged = 1)
#'
#' # All three are mapper output:
#' is_map_df(df1)
#' is_map_df(df2)
#' is_map_df(df3)
#'
#' # Each is the output of one tier:
#' is_map_basic_df(df1)
#' is_map_seq_df(df2)
#' is_map_total_n_df(df3)
#'
#' is_map_basic_df(df2)
#' is_map_seq_df(df3)

is_map_df <- function(x) {
  any(grepl("^scrutiny_.+_map$", class(x)))
}


#' @rdname data-frame-predicates
#' @export

is_map_basic_df <- function(x) {
  is_map_df(x) && !is_map_seq_df(x) && !is_map_total_n_df(x)
}


#' @rdname data-frame-predicates
#' @export

is_map_seq_df <- function(x) {
  any(grepl("^scrutiny_(.+_)?map_seq$", class(x)))
}


#' @rdname data-frame-predicates
#' @export

is_map_total_n_df <- function(x) {
  any(grepl("^scrutiny_(.+_)?map_total_n$", class(x)))
}
