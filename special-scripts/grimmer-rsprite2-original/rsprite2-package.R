#' @import checkmate
#' @importFrom Rdpack reprompt
#' @importFrom stats runif sd
#' @importFrom utils head

## Global variables taken from rSPRITE
# Parameters that trade off speed versus completeness.
# maxDeltaLoopsLower controls how many times we tweak pairs of numbers before giving up hope of finding any solution;
#  it is the lower bound on a formula that includes the sample size and range.
# maxDeltaLoopsUpper is the absolute upper bound on that formula (a sanity check, in effect).
# maxDupLoops controls how many times we try to find another unique solution, when we know that at least one exists.
rSprite.maxDeltaLoopsLower <- 20000
rSprite.maxDeltaLoopsUpper <- 1000000
rSprite.maxDupLoops <- 20

rSprite.dust <- 1e-12 #To account for rounding errors
rSprite.huge <- 1e15 #Should this not be Inf?

#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

.check_req_packages <- function(x, note = "") {
  res <- unlist(suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)))
  if (!all(res)) {
    if (!interactive()) {
      stop(paste0(note, "Some required packages are not installed. Make sure you have
               these packages: ", paste0(x[!res], collapse = ", ")),
           call. = FALSE
      )
    }
    op <- options("warn")
    on.exit(options(op))
    options(warn = 1)
    warning(paste0(note, "The following packages are required for this function but
                   cannot be loaded: ", paste0(x[!res], collapse = ", ")),
            call. = FALSE
    )
    choice <- readline(prompt = "Should I try to install these packages? (Y/N)")
    if (choice %in% c("Y", "y")) {
      utils::install.packages(x[!res])
      res <- unlist(suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)))
      if (!all(res)) {
        stop("Not all packages could be installed successfully. The following could still not be loaded: ", paste0(x[!res], collapse = ", "),
             call. = FALSE
        )
      }
      return(TRUE)
    }
    stop("Cannot proceed without these packages.", call. = FALSE)
  }
}
