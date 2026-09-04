# Documenting exported data -----------------------------------------------

#' Means and sample sizes for GRIM-testing
#'
#' A fictional dataset with means and sample sizes of flying pigs. It can be
#' used to demonstrate the functionality of [`grim_map()`] and functions
#' building up on it.

#' @include utils.R grim-map.R seq-decimal.R
#'
#' @format A tibble (data frame) with 12 rows and 2 columns. The columns are:
#' \describe{
#'  \item{x}{Numeric. Means.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @return A tibble (data frame).
#'
#' @seealso [`pigs2`] for GRIM-testing percentages instead of means, [`pigs3`]
#'   for DEBIT-testing, [`pigs4`] for detecting duplicates, and [`pigs5`] for
#'   GRIMMER-testing.
#'
"pigs1"


#' Percentages and sample sizes for GRIM-testing
#'
#' A fictional dataset with percentages and sample sizes of flying pigs. It can
#' be used to demonstrate the functionality of `grim_map()`, particularly its
#' `percent` argument, and functions building up on it.

#' @include utils.R grim.R seq-decimal.R
#'
#' @format A tibble (data frame) with 6 rows and 2 columns. The columns are:
#' \describe{
#'  \item{x}{Numeric. Percentages.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @return A tibble (data frame).
#'
#' @seealso [`pigs1`] for GRIM-testing means instead of percentages, [`pigs3`]
#'   for DEBIT-testing, [`pigs4`] for detecting duplicates, and [`pigs5`] for
#'   GRIMMER-testing.
#'
"pigs2"


#' Binary means and standard deviations for using DEBIT
#'
#' A fictional dataset with means and standard deviations from a binary
#' distribution related to flying pigs. It can be used to demonstrate the
#' functionality of `debit_map()` and functions building up on it.

#' @include utils.R debit-map.R seq-decimal.R
#'
#' @format A tibble (data frame) with 7 rows and 3 columns. The columns are:
#' \describe{
#'  \item{x}{Numeric. Means.}
#'  \item{sd}{Numeric. Standard deviations.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @return A tibble (data frame).
#'
#' @seealso [`pigs1`] for GRIM-testing means, [`pigs2`] for GRIM-testing
#'   percentages instead of means, [`pigs4`] for detecting duplicates, and
#'   [`pigs5`] for GRIMMER-testing.
#'
"pigs3"


#' Data with duplications
#'
#' A fictional dataset with observations of flying pigs. It contains multiple
#' duplicates. The dataset can be used to demonstrate the functionality of
#' `duplicate_*()` functions such as `duplicate_count()`.

#' @include utils.R duplicate-detect.R
#'
#' @format A tibble (data frame) with 5 rows and 3 columns, describing various
#'   body measures of the fictional pigs. The columns are:
#' \describe{
#'  \item{snout}{Numeric. Snout width.}
#'  \item{tail}{Numeric. Tail length.}
#'  \item{wings}{Numeric. Wingspan.}
#' }

#' @return A tibble (data frame).
#'
#' @seealso [`pigs1`] for GRIM-testing means, [`pigs2`] for GRIM-testing
#'   percentages, [`pigs3`] for using DEBIT, and [`pigs5`] for GRIMMER-testing.
#'
"pigs4"


#' Means, SDs, and sample sizes for GRIMMER-testing
#'
#' A fictional dataset with means, standard deviations (SDs), and sample sizes
#' of flying pigs. It can be used to demonstrate the functionality of
#' `grimmer_map()` and functions building up on it.

#' @include utils.R grimmer-map.R seq-decimal.R
#'
#' @format A tibble (data frame) with 12 rows and 3 columns. The columns are:
#' \describe{
#'  \item{x}{Numeric. Means.}
#'  \item{sd}{Numeric. Standard deviations.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @return A tibble (data frame).
#'
#' @seealso [`pigs1`] for (only) GRIM-testing the same means as here, [`pigs2`]
#'   for GRIM-testing percentages instead of means, [`pigs3`] for DEBIT-testing,
#'   and [`pigs4`] for detecting duplicates.
#'
"pigs5"
