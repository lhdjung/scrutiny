

# Basic function (not exported) -------------------------------------------

proto_split_parens <- function(string, sep = "parens") {

  if (length(sep) == 2) {
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

  sep_close <- paste0(sep_close, ".*")

  out <- stringr::str_split(string, sep_open)
  out <- unlist(out)
  out <- sub(sep_close, "", out)

  divisor <- length(out) / length(string)

  out <- split(out, ceiling(seq_along(out) / divisor))

  return(out)
}



#' Extract substrings from before and inside parentheses
#'
#' @description Two functions that extract substrings from before or inside
#'   parentheses (or brackets, or curly braces): `before_parens()` and
#'   `inside_parens()`.
#'
#'   See `split_by_parens()` to split some or all columns in a data frame into
#'   both parts.
#'
#' @param string Vector of strings with parentheses, brackets, or curly braces.
#' @param sep String. What to split by. Either `"parens"`, `"brackets"`, or
#'   `"braces"`. Default is `"parens"`.
#'
#' @export
#'
#' @return String. The part of `string` before or inside the separating
#'   elements, respectively.


before_parens <- function(string, sep = "parens") {
  out <- proto_split_parens(string, sep)
  out <- purrr::map_chr(out, ~ .[1])
  out <- stringr::str_trim(out)

  return(out)
}


#' @rdname before_parens
#' @export

inside_parens <- function(string, sep = "parens") {

  if (length(sep) == 2) {
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

  sep_close <- paste0(sep_close, ".*")

  out <- proto_split_parens(string, sep)
  out <- purrr::map(out, ~ .[2])
  out <- sub(sep_close, "", out)

  return(out)
}

