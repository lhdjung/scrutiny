# Helpers (not exported) --------------------------------------------------

check_parens_delim <- function(delim) {
  if (!any(length(delim) == c(1L, 2L))) {
    abort_in_export(
      "!" = "`delim` must have length 1 or 2.",
      "x" = "It has length {length(delim)}: {wrap_in_backticks(delim)}."
    )
  }
  # A missing value would fail in stringr, and an empty string matches nothing:
  if (!is.character(delim) || anyNA(delim) || !all(nzchar(delim))) {
    abort_in_export(
      "!" = "`delim` must be a keyword or two strings.",
      "x" = "It is {wrong_spec_string(delim)}."
    )
  }
}


#' Match the `delim` keyword to actual delimiters
#'
#' @description `translate_delim_keywords()` replaces the keywords
#'   `"parens"`, `"brackets"`, or `"braces"` with the characters they describe.
#'   A length-2 `delim` is returned as it is. Anything else is an error.
#'
#'   The delimiters are literal characters, not regular expressions: they are
#'   matched with `stringr::fixed()`.
#'
#' @param delim String (length 1 or 2).
#'
#' @return String (length 2).
#'
#' @noRd
translate_delim_keywords <- function(delim) {
  check_parens_delim(delim)
  if (length(delim) == 2L) {
    delim
  } else if (any(delim == c("parens", "(", "\\("))) {
    c("(", ")")
  } else if (any(delim == c("brackets", "[", "\\["))) {
    c("[", "]")
  } else if (any(delim == c("braces", "{", "\\{"))) {
    c("{", "}")
  } else {
    abort_in_export(
      "!" = "`delim` must be either \"parens\", \"brackets\", or \\
        \"braces\"; or \"(\", \"[\", or \"{{\".",
      "x" = "It was given as {wrap_in_quotes_or_backticks(delim)}.",
      "i" = "Alternatively, choose two custom delimiters; e.g., \\
        `delim = c(\"<\", \">\")` for strings such as \"2.65 <0.27>\"."
    )
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


message_delim_if_cols_excluded <- function(delim) {
  # Translating first also covers spellings such as `"("`:
  delims <- translate_delim_keywords(delim)
  name_delims <- switch(
    paste0(delims, collapse = ""),
    "()" = "parentheses",
    "[]" = "square brackets",
    "{}" = "curly braces"
  )
  if (is.null(name_delims)) {
    msg_delims <- wrap_in_quotes(delims)
    glue::glue("{msg_delims[1L]} and {msg_delims[2L]}")
  } else {
    paste0("i.e., ", name_delims)
  }
}


# Split each string at its first opening delimiter, and the second part at its
# first closing delimiter. Returns a character matrix with two columns: the part
# before the opening delimiter, and the part between the two. Each string is
# split on its own, so that one with more or fewer delimiters can't shift parts
# into another string's row. A string without an opening delimiter has `""` as
# its second part, and a missing one has `NA` as its first.
proto_split_parens <- function(string, delim = "parens") {
  delim <- translate_delim_keywords(delim)
  out <- stringr::str_split_fixed(string, stringr::fixed(delim[1L]), n = 2L)
  out[, 2L] <- stringr::str_split_fixed(
    out[, 2L],
    stringr::fixed(delim[2L]),
    n = 2L
  )[, 1L]
  out
}


# Main functions ----------------------------------------------------------

#' Extract substrings from before and inside parentheses
#'
#' @description `before_parens()` and `inside_parens()` extract the part of a
#'   string before or inside parentheses, brackets, braces, or custom
#'   delimiters. [`split_by_parens()`] does the same for the columns of a data
#'   frame.
#'
#' @param string String vector.
#' @param delim String. The pair of delimiters around the second part:
#'   `"parens"` (the default), `"brackets"`, or `"braces"`, or two custom
#'   strings, matched literally.
#' @param sep `r lifecycle::badge("deprecated")` The old name of `delim`.
#'
#' @export
#'
#' @return String vector as long as `string`, with surrounding whitespace
#'   removed. If a string has no opening delimiter, `before_parens()` returns
#'   all of it and `inside_parens()` returns `NA`, as it does for empty
#'   delimiters.
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
#' inside_parens(string = x)

before_parens <- function(string, delim = "parens", sep = deprecated()) {
  delim <- use_renamed_arg(sep, delim, "before_parens", "sep", "delim")
  stringr::str_trim(proto_split_parens(string, delim)[, 1L])
}


#' @rdname parens-extractors
#' @export

inside_parens <- function(string, delim = "parens", sep = deprecated()) {
  delim <- use_renamed_arg(sep, delim, "inside_parens", "sep", "delim")
  out <- stringr::str_trim(proto_split_parens(string, delim)[, 2L])
  out[out == ""] <- NA
  out
}
