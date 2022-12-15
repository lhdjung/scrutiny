
#' Is an object a consistency test output tibble?
#'
#' @description
#' - `is_map_df()` tests whether an object is the output of a scrutiny-style
#' mapper function for consistency tests, like `grim_map()`. These functions
#' also include those produced by `function_map()`, `function_map_seq()`, and
#' `function_map_total_n()`.
#' - `is_map_basic_df()` is a variant of `is_map_df()` that tests if an object
#' is the output of a "basic" mapper function. This does not include functions
#' produced by `function_map_seq()` or `function_map_total_n()`.
#' - `is_map_seq_df()` tests whether an object is the output of a function that
#' was made by `function_map_seq()`.
#' - `is_map_total_n_df()` tests whether an object is the output of a function
#' that was made by `function_map_total_n()`.
#'
#' @param x Object to be tested.
#'
#' @return Boolean (length 1).
#'
#' @export
#'
#' @examples
#' # Example test output:
#' df1 <- grim_map(pigs1)
#' df2 <- grim_map_seq(pigs1)
#' df3 <- grim_map_total_n(tibble::tribble(
#'   ~x1,    ~x2,   ~n,
#'   "3.43", "5.28", 90,
#'   "2.97", "4.42", 103
#' ))
#'
#' # All three tibbles are mapper output:
#' is_map_df(df1)
#' is_map_df(df2)
#' is_map_df(df3)
#'
#' # However, only `df1` is the output of a
#' # basic mapper...
#' is_map_basic_df(df1)
#' is_map_basic_df(df2)
#' is_map_basic_df(df3)
#'
#' # ...only `df2` is the output of a
#' # sequence mapper...
#' is_map_seq_df(df1)
#' is_map_seq_df(df2)
#' is_map_seq_df(df3)
#'
#' # ...and only `df3` is the output of a
#' # total-n mapper:
#' is_map_total_n_df(df1)
#' is_map_total_n_df(df2)
#' is_map_total_n_df(df3)


is_map_df <- function(x) {
  inherits_class_with(x, "(^scr_)*_map$")
}


#' @rdname is_map_df
#' @export

is_map_basic_df <- function(x) {
  is_map_df(x) && !inherits_class_with(x, "(^scr_)*_map_*.")
}


#' @rdname is_map_df
#' @export

is_map_seq_df <- function(x) {
  inherits_class_with(x, "(^scr_)*_map_seq$")
}


#' @rdname is_map_df
#' @export

is_map_total_n_df <- function(x) {
  inherits_class_with(x, "(^scr_)*_map_total_n$")
}

