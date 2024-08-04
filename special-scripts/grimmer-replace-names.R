
# Load helper functions:
source("special-scripts/replace-in-file.R")

# All variables that should be renamed in scrutiny's version of GRIMMER need to
# be entered here!
grimmer_names <- tibble::tribble(
  ~rsprite2,             ~scrutiny,
  # --- Function ---
  "GRIMMER_test",        "grimmer_scalar",
  # --- Arguments ---
  # (Note that some arguments from `rsprite2::GRIMMER_test()` are missing in
  # scrutiny's adapted version, and vice versa.)
  "mean",                "x",
  "SD",                  "sd",
  "n_obs",               "n",
  "n_items",             "items",
  # --- Internal variables ---
  "decimals_mean",       "digits_x",
  "decimals_SD",         "digits_sd",
  "realmean",            "x_real",
  "realsum",             "sum_real",
  "effective_n",         "n_items",
  "Lsigma",              "sd_lower",
  "Usigma",              "sd_upper",
  "Lowerbound",          "sum_squares_lower",
  "Upperbound",          "sum_squares_upper",
  "possible_integers",   "integers_possible",
  "Predicted_Variance",  "var_predicted",
  "Predicted_SD",        "sd_predicted",
  "Matches_Oddness",     "matches_parity",
  "FirstTest",           "pass_test1",
  "Matches_SD",          "matches_sd",
  "Third_Test",          "pass_test3",
)


# Replace the rsprite2 variable names by those of scrutiny:
replace_from_df(
  path = "R/grimmer-rsprite2.R",
  df_names = grimmer_names,
  col_pattern = "rsprite2",
  col_replacement = "scrutiny"
)

