

#' @include mapper-function-helpers.R
#' @export


audit.scr_audit_seq <- function(data) {
  summarize_audit_special(
    data, selector = starts_with("hits_") | starts_with("diff_")
  )
}

