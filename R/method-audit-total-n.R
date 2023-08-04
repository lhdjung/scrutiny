

#' @include mapper-function-helpers.R
#' @export

audit.scr_audit_total_n <- function(data) {
  audit_summary_stats(
    data, selection = c(
      "hits_total", "hits_forth", "hits_back", "scenarios_total", "hit_rate"
    )
  )
}

