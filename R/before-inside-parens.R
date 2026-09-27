# Helpers (not exported) --------------------------------------------------

check_length_parens_sep <- function(sep) {
  if (!any(length(sep) == c(1L, 2L))) {
    cli::cli_abort(c(
      "!" = "`sep` must have length 1 or 2.",
      "x" = "It has length {length(sep)}: {wrap_in_backticks(sep)}."
    ))
  }
}


#' Match the `sep` keyword to actual separators
#'
#' @description `translate_length1_sep_keywords()` is called within
#'   `split_by_parens()` to replace the legal keywords `"parens"`, `"brackets"`,
#'   or `"braces"` to the characters they describe.
#'
#'   A length-2 `sep` object is returned as it is because it is meant to contain
#'   actual (custom) separators, not a keyword. If `sep` is neither length 2 nor
#'   any of the keywords from above, an error is thrown.
#'
#'   The separators are returned as the literal characters, not as regular
#'   expressions: they are matched with `stringr::fixed()`, so a custom `sep`
#'   such as `c("|", "|")` means the characters themselves.
#'
#' @param sep String (length 1 or 2).
#'
#' @return String (length 2).
#'
#' @noRd
translate_length1_sep_keywords <- function(sep) {
  check_length_parens_sep(sep)
  if (length(sep) == 2L) {
    sep
  } else if (any(sep == c("parens", "(", "\\("))) {
    c("(", ")")
  } else if (any(sep == c("brackets", "[", "\\["))) {
    c("[", "]")
  } else if (any(sep == c("braces", "{", "\\{"))) {
    c("{", "}")
  } else {
    cli::cli_abort(c(
      "!" = "`sep` must be either \"parens\", \"brackets\", or \\
        \"braces\"; or \"(\", \"[\", or \"{{\".",
      "x" = "It was given as {wrap_in_quotes_or_backticks(sep)}.",
      "i" = "Alternatively, choose two custom separators; e.g., \\
        `sep = c(\"<\", \">\")` for strings such as \"2.65 <0.27>\"."
    ))
  }
}


# Warning thrown within tidyselect-supporting functions:
warn_wrong_columns_selected <- function(
  names_wrong_cols,
  msg_exclusion,
  msg_reason,
  msg_it_they = c("It doesn't", "They don't")
) {
  if (length(names_wrong_cols) == 1L) {
    msg_col_cols <- "1 column"
    msg_it_they <- msg_it_they[1L]
    msg_exclusion <- msg_exclusion[1L]
  } else {
    msg_col_cols <- paste0(length(names_wrong_cols), " columns")
    msg_it_they <- msg_it_they[max(1, length(msg_it_they))]
    msg_exclusion <- msg_exclusion[max(1, length(msg_exclusion))]
  }
  names_wrong_cols <- wrap_in_backticks(names_wrong_cols)
  cli::cli_warn(c(
    "!" = "{msg_col_cols} {msg_exclusion}: {names_wrong_cols}.",
    "x" = "{msg_it_they} {msg_reason}."
  ))
}


# This one is only used within `split_by_parens()`:
message_sep_if_cols_excluded <- function(sep) {
  # Translating first covers the legacy spellings such as `"("` as well, which
  # used to fall through every branch and leave the message unfinished:
  seps <- translate_length1_sep_keywords(sep)
  name_seps <- switch(
    paste0(seps, collapse = ""),
    "()" = "parentheses",
    "[]" = "square brackets",
    "{}" = "curly braces"
  )
  if (is.null(name_seps)) {
    msg_seps <- wrap_in_quotes(seps)
    glue::glue("{msg_seps[1L]} and {msg_seps[2L]}")
  } else {
    paste0("i.e., ", name_seps)
  }
}


# Split each string at its first opening separator, and the second part at its
# first closing separator. Returns a character matrix with two columns: the part
# before the opening separator, and the part between the two. Each string is
# split on its own, so that a string with more or fewer separators than the
# others can't shift parts into another string's row, as the former
# `unlist()`-and-rechunk approach did. A string without an opening separator has
# `""` as its second part, and a missing one has `NA` as its first.
proto_split_parens <- function(string, sep = "parens") {
  sep <- translate_length1_sep_keywords(sep)
  out <- stringr::str_split_fixed(string, stringr::fixed(sep[1L]), n = 2L)
  out[, 2L] <- stringr::str_split_fixed(
    out[, 2L],
    stringr::fixed(sep[2L]),
    n = 2L
  )[, 1L]
  out
}


# Main functions ----------------------------------------------------------

#' Extract substrings from before and inside parentheses
#'
#' @description `before_parens()` and `inside_parens()` extract substrings from
#'   before or inside parentheses, or similar separators like brackets or curly
#'   braces.
#'
#'   See [`split_by_parens()`] to split some or all columns in a data frame into
#'   both parts.
#'
#' @param string Vector of strings with parentheses or similar.
#' @param sep String. What to split by. Either `"parens"`, `"brackets"`,
#'   `"braces"`, or a length-2 vector of custom separators. See examples for
#'   [`split_by_parens()`]. Default is `"parens"`. Custom separators are
#'   matched literally, not as regular expressions.
#'
#' @export
#'
#' @return String vector of the same length as `string`. The part of `string`
#'   before or inside the first pair of `sep` elements, respectively, with
#'   surrounding whitespace removed. If a string has no opening `sep` element,
#'   `before_parens()` returns all of it and `inside_parens()` returns `NA`.
#'
#' @name parens-extractors
#'
#' @examples
#' x <- c(
#'   "3.72 (0.95)",
#'   "5.86 (2.75)",
#'   "3.06 (6.48)"
#' )
#'
#' before_parens(string = x)
#'
#' inside_parens(string = x)

before_parens <- function(string, sep = "parens") {
  stringr::str_trim(proto_split_parens(string, sep)[, 1L])
}


#' @rdname parens-extractors
#' @export

inside_parens <- function(string, sep = "parens") {
  out <- stringr::str_trim(proto_split_parens(string, sep)[, 2L])
  out[out == ""] <- NA
  out
}
