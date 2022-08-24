

#' @include mapper-function-helpers.R
#' @export


audit.scr_audit_seq <- function(data) {
  summarize_audit_special(
    data, cols = starts_with("hits_") | starts_with("diff_")
  )
}

