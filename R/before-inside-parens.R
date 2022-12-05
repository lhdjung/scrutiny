

# Basic function (not exported) -------------------------------------------

proto_split_parens <- function(string, sep = "parens") {

  if (length(sep) == 2L) {
    sep_open  <- sep[1]
    sep_close <- sep[2]
  } else {
    if (sep %in% c("parens", "(", "\\(")) {
      sep_open  <- "\\("
      sep_close <- "\\)"
    } else if (sep %in% c("brackets", "[", "\\[")) {
      sep_open  <- "\\["
      sep_close <- "\\]"
    } else if (sep %in% c("braces", "{", "\\{")) {
      sep_open  <- "\\{"
      sep_close <- "\\}"
    } else {
      msg_sep <- paste0("`", sep, "`")
      cli::cli_abort(c(
        "`sep` given as {msg_sep}.",
        ">" = "Please specify `sep` as either \"parens\", \"brackets\", or \\
      \"braces\"; or as \"(\", \"[\", or \"{{\".",
        "i" = "Alternatively, choose two custom separators; e.g., \\
      `.sep = c(\"<\", \">\")` for strings such as \"2.65 <0.27>\"."
      ))
    }
  }

  out <- stringr::str_split(string, sep_open)
  out <- unlist(out)
  out <- sub(paste0(sep_close, ".*"), "", out)

  divisor <- length(out) / length(string)

  split(out, ceiling(seq_along(out) / divisor))
}



#' Extract substrings from before and inside parentheses
#'
#' @description Two functions that extract substrings from before or inside
#'   parentheses, or similar separators like brackets or curly braces:
#'   `before_parens()` and `inside_parens()`.
#'
#'   See `split_by_parens()` to split some or all columns in a data frame into
#'   both parts.
#'
#' @param string Vector of strings with parentheses or similar.
#' @param sep String. What to split by. Either `"parens"`, `"brackets"`,
#'   `"braces"`, or a length-2 vector of custom separators. See examples for
#'   `split_by_parens()`. Default is `"parens"`.
#'
#' @export
#'
#' @return String vector of the same length as `string`. The part of `string`
#'   before or inside `sep`, respectively.
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
  out <- proto_split_parens(string, sep)
  out <- purrr::map_chr(out, function(x) x[1])
  stringr::str_trim(out)
}


#' @rdname before_parens
#' @export

inside_parens <- function(string, sep = "parens") {

  if (length(sep) == 2L) {
    sep_close <- sep[2]
  } else {
    if (sep %in% c("parens", "(", "\\(")) {
      sep_close <- "\\)"
    } else if (sep %in% c("brackets", "[", "\\[")) {
      sep_close <- "\\]"
    } else if (sep %in% c("braces", "{", "\\{")) {
      sep_close <- "\\}"
    }
  }

  out <- proto_split_parens(string, sep)
  out <- purrr::map(out, function(x) x[2])
  sub(paste0(sep_close, ".*"), "", out)
}

