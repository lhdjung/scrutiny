
# Documenting exported data -----------------------------------------------


#' Means and sample sizes for GRIM-testing
#'
#' A fictional dataset with means and sample sizes of flying pigs. It can be
#' used to demonstrate the functionality of `grim_map()` and functions building
#' up on it.

#' @include utils.R grim-map.R seq-decimal.R
#'
#' @format A tibble (data frame) with 12 rows and 2 columns. The columns are:
#' \describe{
#'  \item{x}{String. Means.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @return A tibble (data frame).
#'
#' @seealso `pigs2` for GRIM-testing percentages instead of means, `pigs3` for
#'   DEBIT-testing, and `pigs4` for detecting duplicates.
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
#'  \item{x}{String. Percentages.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @return A tibble (data frame).
#'
#' @seealso `pigs1` for GRIM-testing means instead of percentages, `pigs3` for
#'   DEBIT-testing, and `pigs4` for detecting duplicates.
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
#'  \item{x}{String. Means.}
#'  \item{sd}{String. Standard deviations.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @return A tibble (data frame).
#'
#' @seealso `pigs1` for GRIM-testing means, `pigs2` for GRIM-testing
#'   percentages, and `pigs4` for detecting duplicates.
#'
"pigs3"


#' Data with duplications
#'
#' A fictional dataset with observations of flying pigs. Two pairs of values are
#' duplicates. The dataset can be used to demonstrate the functionality of
#' `duplicate_detect()` and functions building up on it.

#' @include utils.R duplicate-detect.R
#'
#' @format A tibble (data frame) with 7 rows and 3 columns. The columns are:
#' \describe{
#'  \item{x}{String. Means.}
#'  \item{sd}{String. Standard deviations.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @return A tibble (data frame).
#'
#' @seealso `pigs1` for GRIM-testing means, `pigs2` for GRIM-testing
#'   percentages, and `pigs3` for using DEBIT.
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
#'  \item{x}{String. Means.}
#'  \item{sd}{String. Standard deviations.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @return A tibble (data frame).
#'
#' @seealso `pigs1` for (only) GRIM-testing the same means as here, `pigs2` for
#'   GRIM-testing percentages instead of means, `pigs3` for DEBIT-testing, and
#'   `pigs4` for detecting duplicates.
#'
"pigs5"




# # Code for creating the data (normally outcommented) --------------------


# # GRIM for means:
# pigs1 <- tibble::tribble(
#   ~x,       ~n,
#   "7.22",    32,
#   "4.74",    25,
#   "5.23",    29,
#   "2.57",    24,
#   "6.77",    27,
#   "2.68",    28,
#   "7.01",    29,
#   "7.38",    26,
#   "3.14",    27,
#   "6.89",    31,
#   "5.00",    25,
#   "0.24",    28,
# )
#
#
# # GRIM for percentages:
# pigs2 <- tibble::tribble(
#   ~x,
#   "67.4",
#   "54.2",
#   "54.0",
#   "69.8",
#   "68.1",
#   "55.4",
# ) %>%
#   dplyr::mutate(n = 150)
#
#
# # DEBIT:
# pigs3 <- tibble::tribble(
#   ~x,         ~sd,
#   "0.53",     "0.50",
#   "0.44",     "0.50",
#   "0.77",     "0.42",
#   "0.19",     "0.35",
#   "0.34",     "0.47",
#   "0.93",     "0.25",
#   "0.12",     "0.33"
# ) %>%
#   dplyr::mutate(n = 1683)
#
#
# pigs4 <- tibble::tribble(
#   ~snout,  ~tail,
#   4.736,   6.887,
#   8.131,   7.331,
#   4.221,   6.095,
#   4.221,   7.574,
#   5.179,   8.131,
# )
#
#
# # GRIMMER:
# pigs5 <- tibble::tribble(
#   ~x,      ~sd,     ~n,
#   "7.22",  "5.30",  38,
#   "4.74",  "6.55",  31,
#   "5.23",  "2.55",  35,
#   "2.57",  "2.57",  30,
#   "6.77",  "2.18",  33,
#   "2.68",  "2.59",  34,
#   "7.01",  "6.68",  35,
#   "7.38",  "3.65",  32,
#   "3.14",  "5.32",  33,
#   "6.89",  "4.18",  37,
#   "5.00",  "2.18",  31,
#   "0.24",  "6.43",  34
# )





# # Save data:
# usethis::use_data(
#   pigs1,
#   pigs2,
#   pigs3,
#   pigs4,
#   pigs5,
#
#   overwrite = TRUE
# )

