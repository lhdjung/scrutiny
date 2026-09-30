#' @include mapper-function-helpers.R
#' @export

audit.scrutiny_audit_seq <- function(data) {
  data |>
    audit_summary_stats(
      selection = starts_with("hits_") | starts_with("diff_")
    )
}
