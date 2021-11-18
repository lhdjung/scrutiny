

# Basic function (not exported) -------------------------------------------

proto_split_parens <- function(string, sep = "parens") {
  if (sep %in% c("parens", "(", "\\(")) {
    p_open <- "\\("
    p_close <- "\\)"
  } else if (sep %in% c("brackets", "[", "\\[")) {
    p_open <- "\\["
    p_close <- "\\]"
  } else if (sep %in% c("braces", "{", "\\{")) {
    p_open <- "\\{"
    p_close <- "\\}"
  } else {
    cli::cli_abort(c(
      "`sep` given as `{sep}`",
      ">" = "Please specify `sep` as either \"parens\", \"brackets\", or \\
      \"braces\"; or as \"(\", \"[\", or \"{{\"."
    ))
  }

  sep_close <- glue::glue("{p_close}.*")

  s <- stringr::str_split(string, p_open)
  s <- unlist(s)
  s <- sub(sep_close, "", s)
  l <- length(s) / length(string)

  split(s, ceiling(seq_along(s) / l))
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


before_parens <- function(string, sep = "parens") {
  s <- proto_split_parens(string, sep)
  s <- purrr::map(s, ~ .[1])
  s <- as.character(s)

  stringr::str_trim(s)
}


#' @rdname before_parens
#' @export

inside_parens <- function(string, sep = "parens") {

  if (sep %in% c("parens", "(", "\\(")) {
    sep2 <- "\\)"
  } else if (sep %in% c("brackets", "[", "\\[")) {
    sep2 <- "\\]"
  } else if (sep %in% c("braces", "{", "\\{")) {
    sep2 <- "\\}"
  }

  s <- proto_split_parens(string, sep)
  s <- purrr::map(s, ~ .[2])
  s <- sub(glue::glue("{sep2}.*"), "", s)

  as.character(s)
}

