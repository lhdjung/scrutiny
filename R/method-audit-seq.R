

#' @include utils.R
#' @export


audit.scr_audit_seq <- function(data) {

  fn_names <- c(  "mean",      "sd",      "median", "min", "max", "na_count")
  fns      <- list(mean, stats::sd, stats::median,   min,   max,   na_count)

  out <- tibble::tibble()

  for (fn in fns) {
    temp <- dplyr::summarise(data, dplyr::across(
      .cols = starts_with("hits_") | starts_with("diff_"),
      .fns  = fn,
      na.rm = TRUE
    ))
    out <- dplyr::bind_rows(out, temp)
  }

  term <- names(out)
  out <- tibble::as_tibble(t(out), .name_repair = ~ fn_names)

  dplyr::mutate(out, term, .before = 1)
}

