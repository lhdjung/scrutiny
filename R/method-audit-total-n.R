

#' @include mapper-function-helpers.R
#' @export

audit.scr_audit_total_n <- function(data) {
  summarize_audit_special(
    data, selector = c(
      "hits_total", "hits_forth", "hits_back", "scenarios_total", "hit_rate"
    )
  )
}

