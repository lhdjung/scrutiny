
# Helper functions used in the grimmer-replace-names.R file. As part of the
# special-scripts/ directory listed in .Rbuildignore, they are NOT part of the
# built package. See: https://r-pkgs.org/structure.html#sec-rbuildignore

# These functions are taken from an MIT-licensed repo:
# https://github.com/lhdjung/lukas_jung_blog/blob/master/posts/replace-in-file/index.qmd


replace_in_file <- function(path,
                            pattern,
                            replacement,
                            whole_word = TRUE,
                            ignore_case = FALSE,
                            fixed = FALSE,
                            use_bytes = FALSE) {
  # Read the content of the file into a variable:
  file_content_old <- readLines(path)

  if (whole_word) {
    pattern <- paste0("\\b", pattern, "\\b")
  }

  # Replace the word in the content:
  file_content_new <- gsub(
    pattern = pattern,
    replacement = replacement,
    x = file_content_old,
    ignore.case = ignore_case,
    fixed = fixed,
    useBytes = use_bytes
  )

  # Write the updated content back to the file:
  writeLines(file_content_new, path)
}

# Function that goes to the file at `path` and takes `df_names` as a lookup
# table to replace the values in the `col_pattern` column by those in
# `col_replacement`:
replace_from_df <- function(path,
                            df_names,
                            col_pattern,
                            col_replacement,
                            whole_word = TRUE,
                            ignore_case = FALSE,
                            fixed = FALSE,
                            use_bytes = FALSE) {
  if (!all(c(col_pattern, col_replacement) %in% colnames(df_names))) {
    stop("`col_pattern` and `col_replacement` must be column names of `df_names`.")
  }
  for (i in seq_len(nrow(df_names))) {
    replace_in_file(
      path = path,
      pattern = df_names[[col_pattern]][i],
      replacement = df_names[[col_replacement]][i],
      whole_word = whole_word,
      ignore_case = ignore_case,
      fixed = fixed,
      use_bytes = use_bytes
    )
  }
}
