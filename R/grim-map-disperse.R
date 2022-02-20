

# Helper function used within `proto_grim_map_disperse()` below; not exported:
mutate_both_consistent <- function(data) {

  both_consistent <- data$consistency %>%
    split(ceiling(seq_along(data$consistency) / 2)) %>%
    purrr::map_lgl(all) %>%
    rep(each = 2)

  dplyr::mutate(data, both_consistent, .after = "consistency")
}


# Helper function for a single data frame used within `grim_map_disperse()`
# below; not exported:
proto_grim_map_disperse <- function(data, dispersion, n_min, n_max,
                                    x1, x2, items, percent,
                                    show_rec, show_prob, rounding, threshold,
                                    symmetric, tolerance, extra) {

  df_list <- data %>%
    purrr::pmap(
      disperse, dispersion = dispersion, n_min = n_min, n_max = n_max
    ) %>%
    purrr::map(
      grim_map, items = items, percent = percent, show_rec = show_rec,
      show_prob = show_prob, rounding = rounding, threshold = threshold,
      symmetric = symmetric, tolerance = tolerance, extra = extra
    ) %>%
    purrr::map(mutate_both_consistent) %>%
    purrr::map(dplyr::relocate, n_change, .after = both_consistent)

  hits_count <- df_list %>%
    purrr::map(dplyr::filter, both_consistent) %>%
    purrr::map_int(nrow) %>%
    `/`(2) %>%
    as.integer()

  hits_values <- df_list %>%
    dplyr::bind_rows()

  data <- data %>%
    dplyr::mutate(hits = hits_count)

  list(data, hits_values)

}



#' GRIM-testing with varying group sizes
#'
#' @description When reporting group means, some published studies only report
#'   the total sample size but no group sizes corresponding to each mean.
#'   However, group sizes are crucial for GRIM-testing.
#'
#'   In the two-groups case, `grim_map_disperse()` helps in these ways:

#' - It creates hypothetical group sizes by incrementally moving up and down
#' from a user-specified number around half the total sample size. For example,
#' with a total sample size of 40, it will go from 20 to 19 and 21, then to 18
#' and 22, etc.
#' - It GRIM-tests all of these values together with the group means.
#' - It reports the number of scenarios in which both "dispersed" hypothetical
#' group sizes are GRIM-consistent with the group means.
#'
#' All of this works with one or more total sample sizes at a time, each
#' represented by a number around its half. Of course, it is also sensible to
#' test multiple of such numbers with a single set of group means.

#' @param data Data frame with string columns `x1` and `x2`, and numeric column
#'   `n`. The first two are group mean or percentage values with unknown group
#'   sizes, and `n` is a number around half the total sample size. The function
#'   will go up and down from `n` to create hypothetical group sizes.
#'
#'   It is not very important whether a value is in `x1` or in `x2` because,
#'   after the first round of tests, the function switches roles between `x1`
#'   and `x2`, and reports the outcomes both ways.
#' @param dispersion Numeric. Steps up and down from the `n` values. Default is
#'   `0:5`, i.e., `n` itself plus five steps up and down.
#' @param n_min Numeric. Minimal group size. Default is 1.
#' @param n_max Numeric. Maximal group size. Default is `NULL`, i.e., no
#'   maximum.
#' @param x1,x2 Optionally, specify which columns in `data` contain the mean or
#'   percentage values. If not specified here, `data` itself needs to contain
#'   columns by those names. Default is `NULL`.
#' @param show_all Boolean. If set to `TRUE`, the output is a list of tibbles
#'   with detailed analyses. Default is `FALSE`, so that only a single tibble
#'   with summaries is shown.
#' @param
#' items,percent,show_rec,show_prob,rounding,threshold,symmetric,tolerance,extra
#' Arguments passed down to `grim_map()`.

#' @include grim-map.R disperse.R
#'
#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://doi.org/10.1177/09567976211058727
#'
#'   Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
#'   Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://doi.org/10.1177/1948550616673876

#' @seealso `grim_map()`, `disperse()`
#'
#' @return A tibble with summary data:
#' - `x1`, `x2`, and `n` are the inputs.
#' - `hits_total` is the total number of scenarios in which both `x*` values
#'   are GRIM-consistent. Each scenario consists of two `n` values in the
#'   specified `dispersion` range tested against one `x1` and one `x2` value.
#'   Which one of the `x*` values is entered as high or low does not matter
#'   because the tests go both ways.
#' - `hits_forth` is the number of scenarios with both `x*` values consistent
#'   corresponding to `x1` for low `n` and `x2` for high `n` values.
#' - `hits_back`, then, is the number of scenarios in which both of the switched
#'   (role-reversed) `x*` values are consistent.
#'
#' If `show_all` is `TRUE`, this tibble (index `[[1]]`) is part of a list of
#' tibbles in which the other tibbles contain the detailed results.
#'
#' In particular, the first pair of detailed tibbles (index `[[2]]`) contains
#' test results corresponding to the original `x1` and `x2` columns. The second
#' pair (index `[[3]]`) has the values of `x1` and `x2` switched. The summary
#' tibble at the top reports their results as `hits_forth` and `hits_back`,
#' respectively, and adds these two up to `hits_total`.

#' @export

#' @examples
#' # Run `grim_map_disperse()` on data like these:
#' df <- tibble::tibble(
#'   x1 = runif(3, 2, 8) %>% round(2) %>% restore_zeros(),
#'   x2 = runif(3, 2, 8) %>% round(2) %>% restore_zeros(),
#'   n  = runif(3, 30, 60) %>% round()
#' )
#'
#' # The `hits_total` column shows all scenarios in
#' # which both divergent `n` values are GRIM-consistent
#' # with the `x*` values when paired with them both ways:
#' grim_map_disperse(data = df)
#'
#' # By default (`dispersion = 0:5`), the function goes
#' # five steps up and down from `n`. If this sequence
#' # gets longer, the number of hits tends to increase:
#' grim_map_disperse(data = df, dispersion = 0:10)
#'
#' # Get all the details with `show_all = TRUE`:
#' grim_map_disperse(data = df, show_all = TRUE)


grim_map_disperse <- function(data, dispersion = 0:5, n_min = 1, n_max = NULL,
                              x1 = NULL, x2 = NULL, show_all = FALSE,
                              items = 1, percent = FALSE, show_rec = FALSE,
                              show_prob = FALSE, rounding = "up_or_down",
                              threshold = 5, symmetric = FALSE,
                              tolerance = .Machine$double.eps^0.5,
                              extra = Inf) {

  x1 <- rlang::enexpr(x1)
  x2 <- rlang::enexpr(x2)

  # Checks ---

  if (!is.null(x1)) {
    data <- data %>%
      dplyr::rename(x1 = {{ x1 }})
  } else if (!("x1" %in% colnames(data))) {
    cli::cli_abort(c(
      "`x1` column missing",
      ">" = "The column in `data` with the `x` value that hypothetically \\
      corresponds to the lower `n` value needs to be named `x1`, \\
      or else specify the `x1` argument as the name of that column."
    ))
  }

  if (!is.null(x2)) {
    data <- data %>%
      dplyr::rename(x2 = {{ x2 }})
  } else if (!("x2" %in% colnames(data))) {
    cli::cli_abort(c(
      "`x2` column missing",
      ">" = "The column in `data` with the `x` value that hypothetically \\
      corresponds to the higher `n` value needs to be named `x2`, \\
      or else specify the `x2` argument as the name of that column."
    ))
  }


  # Main part ---

  data_forth <- data

  data_back <- data %>%
    dplyr::mutate(
      temp = x2,
      x2 = x1,
      x1 = temp,
      temp = NULL
    )

  out_forth <- proto_grim_map_disperse(
    data = data_forth, dispersion = dispersion, n_min = n_min, n_max = n_max,
    x1 = x1, x2 = x2, items = items, percent = percent,
    show_rec = show_rec, show_prob = show_prob, rounding = rounding,
    threshold = threshold, symmetric = symmetric, tolerance = tolerance,
    extra = extra
  )

  out_back <- proto_grim_map_disperse(
    data = data_back, dispersion = dispersion, n_min = n_min, n_max = n_max,
    x1 = x1, x2 = x2, items = items, percent = percent,
    show_rec = show_rec, show_prob = show_prob, rounding = rounding,
    threshold = threshold, symmetric = symmetric, tolerance = tolerance,
    extra = extra
  )

  hits_forth <- out_forth[[1]]$hits
  hits_back  <- out_back[[1]]$hits

  hits_total <- hits_forth + hits_back

  # This used to be `df_total`:
  df_total <- data %>%
    dplyr::mutate(hits_total, hits_forth, hits_back)

  if (show_all) {
    cli::cli_inform(c(
      "*" = "First table summarizes results.",
      "*" = "Other tables contain detailed analyses.",
      ">" = "(A \"hit\" is a scenario in which both dispersed `n` values \\
      are GRIM-consistent with their corresponding `x*` values.)"
    ))

    out_forth[[1]] <- out_forth[[1]] %>%
      dplyr::rename(hits_forth = hits)

    out_back[[1]] <- out_back[[1]] %>%
      dplyr::rename(hits_back = hits)

    details <- list(out_forth, out_back)

    df_total %>%
      list() %>%
      append(details)

  } else {
    df_total
  }

}


