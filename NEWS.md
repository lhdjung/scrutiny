# scrutiny 1.0.0

scrutiny 1.0.0 is a major release with many breaking changes. If you have used GRIM, GRIMMER, or DEBIT before, expect to adjust your code; the error messages say how.

The consistency tests now use exact integer arithmetic, several rounding procedures were corrected, and all mappers are made by `function_map()`. The rounding functions moved to the [roundwork](https://lhdjung.github.io/roundwork/) package. Under the default settings, GRIM's verdicts on testable value sets are unchanged, GRIMMER no longer flags many consistent value sets and catches some it used to miss, and DEBIT is now exact. See "Test results" for details.

## Breaking changes

### Numeric inputs

- `x` and `sd` are now numeric in `grim()`, `grimmer()`, `debit()`, and their mappers; strings are no longer accepted. Since numbers drop trailing zeros, the new `digits_x` and `digits_sd` arguments are required. They take one value per row in the basic mappers and one per group in the total-n mappers, as in `grim_map_total_n(df, digits_x = c(2, 1), items_averaged = 1)`, and must be whole numbers from 0 to 15. Together with the integer digits of `x` or `sd`, they may not exceed the 15 significant digits that a double holds.

- Mappers return `digits_x` and `digits_sd` as columns. `grim_plot_grid()` (formerly `grim_plot()`) reads the number of decimal places from them instead of inferring it from `x`.

- The `n` column of mapper output is an integer column if all its values are whole numbers within the integer range.

### Test results

- GRIMMER tests every sum that rounds to the reported mean. Earlier versions only took the one nearest to `x * n`, so tests 1 and 3 could fail for a consistent value set whenever several sums round to the mean, which can happen once `n` reaches `10^digits_x`. In a grid of such value sets, a fifth of the inconsistencies that scrutiny 0.6.1 found for means with one decimal place were false positives, as were 3% of those for means with two decimal places. The warning about this was removed. Thanks to Nathanael Larigaldie and \@takua624 (#80, #84).

- GRIMMER computes candidate sums and sums of squares in exact integer arithmetic. Floating-point comparisons could lose a candidate on a rounding bound, so some consistent value sets were reported as inconsistent, also for multi-item scales. For example, `grimmer(x = 0.03, sd = 0.18, digits_x = 2, digits_sd = 2, items_averaged = 1, n = 200)` was `FALSE`, although seven 1s and 193 0s have exactly these statistics. Thanks to Ian Hussey (#86) and Aurélien Allard.

- GRIMMER rejects sums of squares below the minimum that whole numbers with the reconstructed sum can have. For example, `grimmer(x = 3.6, sd = 0.2, digits_x = 1, digits_sd = 1, items_averaged = 1, n = 11)` was `TRUE`, but 11 whole numbers that sum to 40 cannot have an SD below 0.5.

- For `n < 5`, GRIMMER also rejects sums of squares that are large enough but unattainable: `grimmer(x = 1.0, sd = 2.0, digits_x = 1, digits_sd = 1, items_averaged = 1, n = 2)` was `TRUE`, but the values would be 1 ± √2. For `n >= 5`, the minimum and the parity test suffice, so `grimmer()` is `TRUE` if and only if a sample of whole numbers has the reported mean and SD, except under `rounding = "even"`, which lets a tie round either way. The `reason` for both new rejections is `"GRIMMER inconsistent (test 1)"`.

- A rounding bound is exclusive whenever a value exactly on it would round away from the reported number. This already held for `"up"` and `"down"`, and now also for `"ceiling"`, `"floor"`, `"trunc"`, `"anti_trunc"`, and the `"*_from"` methods. Under `"even"`, both bounds stay inclusive, because whether `base::round()` sees a tie depends on the binary representation of the value.

- `symmetric` now affects the results of `grim()` and `grimmer()`, not only the `show_rec` values. `threshold` no longer affects `"up"`, `"down"`, and `"up_or_down"`, which always round from 5.

- `"down_from"` and `"up_from_or_down_from"` read `threshold` as `"up_from"` does: rounding goes down if the part cut off is at most `threshold` tenths of a rounding step. It used to switch at `10 - threshold`, so `"up_from_or_down_from"` could span up to 1.8 steps, and GRIM with `threshold = 9` accepted every mean at `n = 57`. Results at the default `threshold = 5` are unchanged.

- `"anti_trunc"` no longer moves a value that already has the required number of decimal places: 8.42 rounded to two decimal places is 8.42, not 8.43, as with Excel's `ROUNDUP()`. A mean of `0.00` is therefore consistent under `"anti_trunc"`; it used to be `FALSE`.

- DEBIT is exact by default. The mean of `n` binary values is `k / n` for a whole number `k`, but DEBIT treated it as continuous and accepted some SDs that no binary sample of size `n` can have. The new `formula` argument selects `"exact"`, the default, or `"mean_n"`, the old behavior. With `show_rec = TRUE`, `debit_map()` also returns `sum_lower` and `sum_upper`, the smallest and largest `k` whose mean would be reported as `x`.

- GRIM, GRIMMER, and DEBIT return `NA` for every case that cannot be tested: a missing or infinite value, including a missing `digits_*` value, or an `n` or `items_averaged` that is not a positive whole number. GRIMMER and DEBIT also need `n >= 2`, since a sample SD divides by `n - 1`, unless `sd_meaning = "sd_pop"`. Such cases used to cause an error or return an arbitrary `TRUE` or `FALSE`. The renamed version of `grim_probability()`, `grim_exp_incon()` returns `NA` for them too.

- `grim_exp_incon()`, `grim_total()`, and the `exp_incon` column of `grim_map()` take the rounding method into account via new `rounding` and `threshold` arguments. The old formula, `(10^D - N * L) / 10^D`, assumes that each reported value comes from a single range of unrounded values, one step wide. Under the default `"up_or_down"`, a tie is accepted both ways, so the probability is lower: `0.4` instead of `0.6` for `n = 40` and two decimal places.

### Function interfaces

- `grim_probability()` is now `grim_exp_incon()` (the old name is deprecated), and the `probability` column of `grim_map()` is now `exp_incon`, the expected rate of inconsistency. In every `audit()` method for mapper output, including `audit_cols_minimal()`, the first columns are now `all_cases`, `incon_cases` (formerly `incons_cases`), and `emp_incon` (formerly `incons_rate`), in this order. For `grim_map()` output, `testable_cases` follows `all_cases`, and `exp_incon` (formerly `mean_grim_prob`) and `ratio` (formerly `incons_to_prob`) come last. `testable_rate` was removed: it is `testable_cases / all_cases`.

- `items` is now `items_averaged` and has no default (#94). It is the number of items averaged into each score; the old name was mistaken for the number of scale points, and the old default of 1 silently tested means of multi-item scales as single items. Give it wherever it applies, as in `grim()`, `grimmer()`, their mappers, `grim_values()`, and `grim_exp_incon()`; use `items_averaged = 1` for single items and percentages. Mappers also accept an `items_averaged` column. `grim_granularity()` keeps its default of 1.

- In every mapper, the dots come right after the key arguments, so the arguments after them must be named in full: `items = 2` is now an error rather than a partial match. The total-n mappers gain an `n` argument for the column of totals, as in `grim_map_total_n(df, digits_x = c(2, 1), n = "total", items_averaged = 1)`.

- scrutiny uses the base pipe `|>`, no longer exports `%>%`, and requires R >= 4.1.0.

- `grim_map()`, `grimmer_map()`, and `debit_map()` are made by `function_map()` and have an explicit argument for every argument of `grim()`, `grimmer()`, and `debit()`, such as `debit_map(formula)`. A `digits_*` column of `data` that disagrees with the argument is an error, as are two key arguments that name the same column. `merge_items`, `extra`, and `testables_only` were removed: use `dplyr::select()` instead of `extra` and `dplyr::filter(exp_incon > 0)` instead of `testables_only`. `grim_map(percent = TRUE)` no longer prints a message.

- The sequence and total-n mappers, such as `grim_map_seq()`, have an explicit argument for every setting of their basic mapper, such as `rounding` or `sd_meaning`, with the same default. These used to be passed through the dots, which must now be empty. Existing calls are unaffected.

- `debit_map()`'s `show_rec` defaults to `FALSE`, as in `grim_map()`; `debit_plot()` reconstructs the values itself. `debit_map()` output no longer has a `rounding` column. `grimmer_map()` no longer has `show_reason`: the `reason` column is always returned. `audit()` on `grimmer_map()` output no longer has a `fail_test2` column.

- `audit()` on sequence or total-n mapper output calls `audit_seq()` or `audit_total_n()`. It used to apply the basic mapper's summary, which counted the dispersed rows as reported cases. `grim_binomial()` and `grim_plot_rates()` reject such output for the same reason. `audit_cols_minimal()` lost its `name_test` argument.

- Mappers accept any data frame and return a tibble. Character row names, such as study labels, are kept as a `rowname` column.

- `grim()`, `grimmer()`, and `debit()` lost `show_rec` and `show_reason`, which were only meant for the mappers. They are vectorized over `x`, `sd`, `n`, the `digits_*` arguments, and `items_averaged`; all other arguments must have length 1. Recycling follows the tidyverse rules, so `grim(x = c(5.19, 5.18, 5.17), n = c(28, 32), digits_x = 2, items_averaged = 1)` is an error.

- `grim_map()` and `grimmer_map()` no longer multiply `items` into `n`: `n` is the sample size as given, and `items_averaged` is a column of its own. `grim_plot_grid()` places tiles at `n * items_averaged`.

- `grim_map(show_rec = TRUE)` returns the same five columns for every rounding method: `rec_sum`, `sum_lower`, `sum_upper`, `rec_x_upper`, and `rec_x_lower`. The `rec_x_*_rounded*` columns are gone. `sum_lower` and `sum_upper` are the smallest and largest whole-number sums that would be reported as `x`, and `consistency` is `TRUE` exactly if `sum_lower <= sum_upper`, so the columns cannot contradict the result. With `percent = TRUE`, `rec_x_upper` and `rec_x_lower` are percentages, like `x`.

- S3 classes start with `scrutiny_` instead of `scr_`, as in `"scrutiny_grim_map"` (#39).

- `function_map()`, `function_map_seq()`, and `function_map_total_n()` take the function to map as `.fn` instead of `.fun`.

- Mapper settings that other functions need later, such as `rounding`, are recorded in a `"scrutiny"` attribute instead of S3 classes, which are now used for dispatch only. The `"scr_rounding_*"`, `"scr_percent_true"`, `"scr_map_seq_disp_nonlinear"`, `"scr_seq_df"`, and `"scr_seq_test"` classes are gone.

- In `*_map_seq()` output, `case` is the row number of the value set in the input data; it used to count only the inconsistent rows. `n` and `items_averaged` are separate columns, also in `*_map_total_n()` output, and `n` is dispersed in steps of 1.

- In every function, `rounding` must be a string and `symmetric` a single `TRUE` or `FALSE`; a numeric or logical `rounding` was silently read as a position in the list of rounding methods.

- `decimal_places()`, `decimal_places_df()`, `restore_zeros()`, and `restore_zeros_df()` match `sep` literally, and the default is `"."` instead of `"\\."`.

- `grim_plot_grid()` and `debit_plot()` return their plot instead of printing it. `grim_plot_grid()` has an aspect ratio of 1 (undo it with `ggplot2::theme(aspect.ratio = NULL)`) and a new `split_by_digits` argument that returns one plot per number of decimal places. `debit_plot()` lost `line_size`, `show_outer_boxes`, and the `tile_*` arguments. Instead, each rectangle is drawn over a point at the reported mean and SD, so that value sets with tiny rectangles stay visible.

- `duplicate_count_colpair()` requires at least two columns, and its `rate_y` is the proportion of `y` values that occur in `x`, as documented.

## New features

- `grim_explain()`, `grimmer_explain()`, and `debit_explain()` lay out the test of a single case in plain words. They are built from the numbers that decide the test, so they always agree with `grim()`, `grimmer()`, and `debit()`. `explain_map()` adds these explanations to mapper output as an `explanation` column, using the settings of the mapper call, and `explain_print()` prints them.

- `grimmer()`, `debit()`, and their mappers, explanations, and plots have a new `sd_meaning` argument: what a value reported as an SD really is. `"sd_sample"`, the default, is a sample SD, which divides by `n - 1`. `"sd_pop"` is a population SD, which divides by `n`, as with NumPy's `np.std()` or Excel's `STDEV.P`. `"sd_sample_or_pop"` accepts a value set that is consistent under either; `grimmer_map()`'s `reason` column names the one that passed. `"se"` is a standard error reported as an SD. This test is weak: from about `n = 20` on, nearly every value set whose mean passes GRIM is consistent under `"se"`. Thanks to Ian Hussey (#95).

- `is_percentage_of()`, `is_percentage_of_map()`, and `is_percentage_of_map_seq()` check whether a reported percentage matches a count out of a total. `add_up_to()` and `add_up_to_map()` check whether subgroup sizes sum to a total; `add_up_to_map()` selects any number of columns with tidyselect. `is_percentage_of_map()` and `add_up_to_map()` have `audit()` methods.

- `grim_plot_values()` and `grimmer_plot_values()` visualize the test of a single value set. They show every mean (or SD) within a range that could be reported with the given number of decimal places, mark those that a sample of the given size can have, and indicate the reported value.

- `grim_plot_rates()` plots each study's rate of GRIM-inconsistent value sets against the rate that random means would show.

- `grim_values()` returns all means of `n` integers that would be reported as `x`, with the whole-number sum behind each, and `grim_closest()` the achievable mean closest to `x` (#62).

- `grimmer_values()` returns every pair of a mean and an SD of `n` integers that would be reported as `x` and `sd`, with the sum and sum of squares behind each. This is related to #59 but doesn't conclude the questions raised there.

- `grim_binomial()` tests whether the rate of GRIM-consistent value sets is above what random decimal digits would produce, and returns a Clopper-Pearson confidence interval. `grim_binomial_power()` computes the power of this test.

- `audit()` has a `.by` argument for summaries by group, as in `audit(out, .by = study)`. Output grouped by `dplyr::group_by()` now gets an error that explains the problem, instead of "no applicable method". Methods in other packages need the dots: `audit.my_class <- function(data, ...)`.

- `scrutiny_meta()` returns the settings that a mapper recorded on its output, such as `rounding` and `sd_meaning`, so that a report can state the conditions of a verdict. Printed mapper output names the settings that differ from the defaults in its header, as in `# GRIM: rounding = "up"`.

- New `rounding` strings `"ties_up"`, `"ties_down"`, `"ties_away"`, `"ties_zero"`, and `"ties_even"`. Each names a complete tie-breaking rule (`"ties_away"` is `"up"` with `symmetric = TRUE`), so combining one of these strings with `symmetric` is an error.

- `function_map()` gains `.args_by_row`, `.args_fixed`, `.cols_helper`, `.cols_derived`, a working `.col_names`, and `.reported_variadic` for tests in which the data determine the number of key columns (#42). `function_map_seq()` gains `.var_bounds`, which sets the dispersion limits per variable, so that a mean can go below zero while `n` stays at least 1.

- All functions new in 1.0.0 are experimental. The developer tools, such as `function_map()`, may change in minor releases.

## Bug fixes

### Consistency tests

- `debit()` no longer reports consistent binary data with a mean of exactly `0.50` as inconsistent, and returns a result for means of exactly 0 and 1, which used to be `NA`.

- Under `rounding = "up"` or `"down"`, GRIMMER's SD check and parity check now apply to the same candidate sum of squares, so `grimmer(x = 0.11, sd = 0.87, digits_x = 2, digits_sd = 2, items_averaged = 1, n = 64, rounding = "up")` is `FALSE`. Thanks to Ian Hussey (#85).

- `grimmer()` gives the reason `"Negative SD"` for a negative `sd`. Instead of running indefinitely for a very large `n`, it throws an error if more than a million integer sums are compatible with the reported mean.

- `audit()` no longer counts a case without a test result as inconsistent, and computes `exp_incon`, `ratio`, `mean_x`, and `mean_sd` over the cases with a result instead of returning `NA` if one has none.

### Sequence and total-n mappers

- An error in a row of `data` names that row instead of coming wrapped in purrr's "In index" context. In the total-n mappers, the row is that of the reported values, not of a hypothetical scenario.

- `reverse_map_seq()` and `audit_seq()` reconstruct the reported values from the `diff_var` column. They used to infer them from the gap in the sequence, which failed when `out_min` or `out_max` cut it short, so that `audit_seq()` could report the opposite result.

- `audit_seq()` reports the verdicts that the sequence mapper recorded instead of re-testing reconstructed data, which could drop `percent`, `threshold`, or `symmetric` and flip the verdict. It works with a `dispersion` of length 1, with output that has no rows, and with cases whose sequence `out_min` or `out_max` emptied. A consistent reported value set included by `include_reported = TRUE` counts once in `hits_total`, not once per dispersed variable.

- `*_map_seq()` functions disperse means and SDs down to zero, and means below it; they used to stop one decimal unit above zero. With `include_consistent = TRUE`, they skip cases without a test result. If there is nothing to disperse, they warn and return a tibble with no rows but the usual columns. `diff_var` is an integer column.

- `seq_disperse()` rounds its values to the decimal level of `by`, so that long sequences don't accumulate floating-point error (#83). It applies `out_min` and `out_max` to `from` as well, and no longer includes `from` twice when `dispersion` contains 0.

- `*_map_total_n()` functions use an `items_averaged` column, and `reverse_map_total_n()` reconstructs the correct total for any `dispersion`. Functions made by `function_map_total_n()` work when created in another package (#69). An even total split into two equal groups is tested once rather than twice, so `audit_total_n()` no longer counts one split too many. `audit_total_n()` leaves undecidable splits out of `scenarios_total`, and its `hit_rate` is `NA` for a case with no decidable split.

- `*_map_total_n()` functions return the other columns of `data`, and warn about a total that leaves no pair of group sizes within `n_min` and `n_max` instead of dropping it silently. A helper argument such as `items_averaged` must be a single value there; a vector was paired with the wrong groups in the `"back"` direction. The `constant` values stay with their groups in the `"back"` direction, too. Input columns named like the result columns (`n_change`, `both_consistent`, `case`, `dir`, and `constant` if given) or the reported values (`x`, `sd`) are an error instead of being overwritten. Grouped input is tested like ungrouped input.

- `grimmer_map_seq()` and `debit_map_seq()` no longer disperse `n` below 2, and `grimmer_map_total_n()` and `debit_map_total_n()` default to `n_min = 2`, since a group of 1 has no sample SD. Under `sd_meaning = "sd_pop"` or `"sd_sample_or_pop"`, where it has one, both tiers go down to 1.

### Decimal places

- `decimal_places()` handles scientific notation, counts numeric values correctly when `sep` is not `"."`, and returns `NA` for `NaN`, infinite values, and the string `"NA"`.

- `restore_zeros()` handles scientific notation, reads a number as `decimal_places()` does (so `1e-300` has 300 decimal places, not 315), no longer introduces floating-point noise at high widths, and returns `NA` instead of `"NA"`. It rejects a negative `width` and reports values that exceed a per-value `width`. `restore_zeros_df()` and `decimal_places_df()` handle columns that contain only `NA`, and recognize columns whose decimal separator is `sep_in` or `sep`. `restore_zeros_df()` no longer warns about a column without decimal places, such as `n`, or a single row.

### Other

- `split_by_parens()`, `before_parens()`, and `inside_parens()` split each string separately; a string with an unusual number of separators used to shift the values of all later rows. Custom separators are matched literally. `split_by_parens()` errors instead of overwriting a column, handles `NA` and input without rows, only warns about unsplit columns that `cols` selected, and keeps the column order with `transform = TRUE`.

- `row_to_colnames()` keeps a one-column data frame a data frame, and rejects a header that repeats, such as "M, SD, M, SD".

- `duplicate_tally()` no longer counts missing values as matches of every other value, runs in linear rather than quadratic time, and treats `NaN` as missing. `duplicate_count()` uses a named vector's names as `locations`, drops unused factor levels, and works on grouped tibbles. `duplicate_count_colpair()` compares values as strings. All `duplicate_*()` functions write numbers out in full, so `100000` is the same value as `100000L` and `"100000"`, not `"1e+05"`.

- `is_seq_linear()` returns `FALSE` for sequences such as `c(1, 2, 1)`. The `is_seq_*()` functions fill gaps left by `NA` values using the step size implied by the known values, return `NA` for a vector of only `NA` unless the answer is the same whatever the values, and test strings and factors by their numeric values.

- `grim_plot_grid()` plots negative means at the fractional part of their absolute value, plots percentages with the decimal places they were tested with, works with `show_raster = FALSE`, extends its x-axis to the data when `n` is not given, uses its `rounding` argument for data without a recorded rounding method, and warns about any rows it leaves out instead of suppressing all warnings. It only says "Also visualizing" GRIMMER inconsistencies if there are GRIM inconsistencies as well.

- `debit_plot()` draws the DEBIT lines for the smallest and largest sample size and shades the band between them; it used to draw a single line from a recycled `n` column. Its x-axis is labeled as the mean, and `data` without rows is an error.

- Mappers can be called via `lapply()` or `purrr::map()`, and a key column name such as `x = "mean"` can be passed in a variable. `.name_key_result` works in all three function factories. `function_map_seq()` works for a mapper with a single reported variable. `is_map_df()` and the related predicates recognize only scrutiny's own classes; they used to accept any class ending in `_map`.

- `reverse_map_total_n()` rejects mapper output with rows removed or reordered. `disperse()` rejects a missing `n_max`, and `disperse2()` applies `n_min` and `n_max` to the group sizes rather than to their midpoint. `seq_disperse()` disperses a factor by its label. The `sd_binary_*()` functions return `NA` for `n` below 2.

- `grim()` and `grim_map()` no longer fail when `n` and `items_averaged` are integers whose product exceeds the integer range.

- Errors about an invalid `rounding` or `symmetric` name the function that the user called. Many errors from base R or from internal code were replaced by messages that name the argument at fault. `audit()` and `grim_plot_grid()` name the columns they need if any were dropped.

- `restore_zeros_df()`, `decimal_places_df()`, and `split_by_parens()` ignore grouping and return ungrouped data; grouped input used to fail or leave columns unchanged. `is_numeric_like()` is `FALSE` for dates and times, which `restore_zeros_df()` used to turn into `NA`.

- `grim_map()` is about 2.5 times as fast, and `grimmer_map()` about 80 times.

## Lifecycle changes

- The rounding functions moved to the roundwork package, which scrutiny now imports: `reround()`, `unround()`, `reround_to_fraction()`, `reround_to_fraction_level()`, `rounding_bias()`, `anti_trunc()`, and the `round_*()` functions. Call them from roundwork, as in `roundwork::round_up()`; see roundwork's NEWS for their changes. `vignette("rounding-options", package = "roundwork")` and `vignette("rounding-in-depth", package = "roundwork")` moved with them. Errors about `rounding`, `threshold`, and `symmetric` still name the scrutiny function that was called, and are now raised even if `data` has no rows.

- `grim_plot()` is deprecated in favor of `grim_plot_grid()`, which tells it apart from `grim_plot_values()` and `grim_plot_rates()`.

- `grim_items()` is deprecated. It only computed `1 / (n * gran)`.

- `grim_probability()` is deprecated in favor of `grim_exp_incon()`.

- Removed after their deprecation in 0.5.0: `grim_map()`'s `show_prob`, `grim_ratio_upper()`, and the 15 functions around `is_subset_of()`.

- Removed: `grim_ratio()` (use `grim_exp_incon()`, which takes the rounding method into account), `manage_key_colnames()` (use `absorb_key_args()`), `unnest_consistency_cols()`, `write_doc_factory_map_conventions()`, and the re-exports of rlang's `enquo()`, `enquos()`, `as_name()`, `as_label()`, `.data`, and `:=`. `check_audit_special()` was removed, since `audit()` now reaches `audit_seq()` and `audit_total_n()` itself. `check_factory_dots()`, `check_args_disabled()`, `decimal_places_scalar()`, `write_doc_audit()`, `write_doc_audit_seq()`, and `write_doc_audit_total_n()` are no longer exported.

- `duplicate_detect()`, superseded since 0.3.0, was removed in favor of `duplicate_tally()`.

- `debit_plot()` and `grim_plot_grid()` map `consistency` to one scale for `colour` and `fill`, so that `ggplot2::scale_colour_manual()` with `aesthetics = c("colour", "fill")` restyles them. The arguments for fixed styles were removed: `debit_plot()`'s `color_cons`, `color_incons`, `rect_alpha`, `line_*`, and `label_*`, and `grim_plot_grid()`'s `color_cons`, `color_incons`, `tile_alpha`, and `raster_alpha`.

- `seq_disperse()` and `seq_disperse_df()` return numbers, not strings, and lost their `string_output` argument. A string `from` still sets the step size by its decimal places, trailing zeros included.

- `seq_length()` and `seq_length<-` were removed; their names clashed with `seq_len()`. So were `seq_endpoint()`, `seq_distance()`, `seq_endpoint_df()`, `seq_distance_df()`, and `seq_test_ranking()`, which made decimal strings for mappers that no longer take strings. Use `seq(from, to, by = 10^-digits)`.

- Removed after their deprecation in 0.3.1: `seq_disperse()`'s `track_var_change` and `seq_disperse_df()`'s `.track_var_change`. `restore_zeros_df()` and `split_by_parens()` lost `...`, which only caught argument names from before 0.2.4.

- `grim()`, `grim_map()`, `grimmer()`, and `grimmer_map()` lost `tolerance`: both tests compare whole numbers exactly. GRIMMER's test 2 no longer rounds reconstructed SDs, since every sum of squares that passes test 1 passes it, so `"GRIMMER inconsistent (test 2)"` is no longer a possible `reason`.

- `function_map()` lost `.col_control` and `.col_filler`, which never worked.

- `disperse()`, `disperse2()`, `disperse_total()`, and the `*_map_total_n()` functions lost `constant_index`, and `function_map_total_n()` lost `.constant_index`. The `constant` columns always follow `n_change`; move them with `dplyr::relocate()`.

- scrutiny requires purrr >= 1.0.0 (#87) and ggplot2 >= 3.4.0, no longer depends on corrr, and suggests ggrepel instead of importing it.

## Documentation

- `vignette("consistency-tests-in-depth")` was updated (#74), the GRIMMER documentation was improved (#66), and "Related software" has new entries.

# scrutiny 0.6.1

This is a fix for compatibility with testthat 3.3.0. Also:

## Bugfixes

-   Fixed a bug in `grimmer_map_total_n()`, `debit_map_total_n()`, and the underlying `function_map_total_n()` (#81). This could also have lead to incorrect results, but only in total-N mapper functions with two or more key arguments beyond `n`, so the bug never affected `grim_map_total_n()`.

-   Added a warning whenever GRIMMER runs. I recently learned about a bug that can lead to false positives in test 3 of the current GRIMMER implementation; see #80. The bug will be fixed in the next version of scrutiny (and the warning will be removed).

## Minor changes

-   Updated the ["Related software"](https://lhdjung.github.io/scrutiny/articles/related.html) article to reflect recent additions to the forensic metascience toolkit. Now, it also attempts to roughly categorize tools by usage.

# scrutiny 0.6.0

## Bugfixes

-   Fixed a bug in the DEBIT functions that could sometimes have resulted in `consistency` being `FALSE` when it should have been `TRUE` (thanks to \@nrposner, #75). However, this seems to be a rare issue, and DEBIT is not widely used in any case.

-   Fixed a bug that could theoretically lead `grim()`, `grim_map()`, `grim_map_seq()`, and `grim_map_total_n()` to throw a warning and possibly even return incorrect results (also \@nrposner, #75). However, this is even less realistic than the previous bug.

-   Compatibility with ggplot2 4.0.0 was ensured (@teunbrand, #78).

-   `restore_zeros()` now checks `width` more strictly:

    -   It no longer truncates decimal numbers if `width` is specified but some elements of `x` have more decimal places than that. For example, in earlier versions, `restore_zeros(c(0.12, 0.123, 0.1234), width = 2)` would have returned `c("0.120", "0.123", "0.123")`: it silently cut off the `4` from the last value. An error is now thrown in such cases.
    -   Also, `width` is now checked to be either a single whole number or a vector of whole numbers with the same length as `x`.

-   `restore_zeros_df()` has the same fixes as above.

-   `is_seq_dispersed()` now works correctly if `NA` values are present.

## Lifecycle updates

Currently none. However, the next major or minor version will introduce breaking changes, removals (as announced in scrutiny 0.5.0 below), and possibly more deprecations.

# scrutiny 0.5.0

The package is now released under the MIT license.

## Breaking changes

-   The `ratio` column in the output of `grim_map()` and `grim_map_seq()` was replaced by a `probability` column. This means:

    -   Numerically, the only difference is that `probability` is zero whenever `ratio` was negative.

    -   Conceptually, it is much easier to interpret: it is the probability that a reported mean or percentage of integer data that has a specific number of decimal places but is otherwise random is GRIM-inconsistent with the reported sample size.

        For example, `probability` is `0.6` for a mean of `1.23` and a sample size of `40`. The same is true for any other mean with two decimal places. Thus, a randomly chosen mean with two decimal places, ostensibly derived from integer data, has a 0.6 probability of being GRIM-inconsistent with the reported sample size.

-   In the functions around `grim_ratio()`, the `x` argument must now be a string. This is consistent with `grim_map()`, `unround()`, etc.; and it prevents erroneous results that could previously occur by omitting trailing zeros.

-   The GRIMMER implementation was debugged, so that `grimmer_map()` etc. may now yield different results in a few cases. In particular, the `items` argument now works correctly, thanks to Aurélien Allard and Lukas Wallrich (#58).

-   `is_seq_dispersed()` now correctly returns `FALSE` if different numbers of missing values at the start and end of `x` mean that `x` cannot be dispersed around `from`.

## New features

-   The `probability` column (see above) is created by a new function, `grim_probability()`.

## Lifecycle updates

### Deprecated

-   As a consequence of the above, the `show_prob` argument of `grim_map()` is now deprecated and will be removed in a future version. It no longer has any effect.

-   `grim_ratio_upper()` is deprecated and will be removed in a future version. It no longer seems very interesting (and likely never was), especially now that the GRIM ratio in general has taken a backseat.

-   All 15 (!) functions around `is_subset_of()` are deprecated and will be removed in a future version. In truth, they were always poorly written and widely out of scope for scrutiny.

### Removed

All of these had been deprecated since scrutiny 0.3.0:

-   `audit_list()` was removed.

-   The `sep` argument in `restore_zeros()` and `restore_zeros_df()` was removed.

-   The `numeric_only` argument in `duplicate_count()` and `duplicate_detect()` was removed.

-   The `na.rm` argument in `duplicate_count_colpair()` was removed.

# scrutiny 0.4.0

This version brings major performance improvements. Furthermore:

## Bugfixes

-   Fixed a bug in `audit_seq()`: If the `dispersion` argument in the preceding call to a function like `grim_map_seq()` was specified as something other than a linearly increasing sequence, the `"diff_*"` columns in the data frames returned by `audit_seq()` may have contained incorrect values.
-   Similarly, `audit_seq()` and `reverse_map_seq()` used to reconstruct the reported values incorrectly if the `dispersion` default was overridden as described above. At least for now, the issue is handled by throwing an error if these functions operate on data frames that are the result of specifying `dispersion` as something other than a linearly increasing sequence.
-   Fixed a bug that incorrectly threw an error in `grim_map_seq()`, other functions made by `function_map_seq()`, as well as `seq_disperse()` and `seq_disperse_df()` if an input value was so close to `out_min` or `out_max` that the output sequence would be shorter than implied by `dispersion` / `.dispersion` , and if `track_var_change` / `.track_var_change` (see below) was `TRUE`. Again, note that the bug only occurred if an error was thrown.

## New features

-   A new vignette lists the options for specifying the `rounding` argument that many scrutiny functions have: `vignette("rounding-options")`.
-   Another new vignette shows the minimal steps to implement a consistency test using scrutiny: `vignette("consistency-tests-simple")`.
-   The output of `grim_map_seq()`, `grimmer_map_seq()`, `debit_map_seq()` and any other function made by `function_map_seq()` now has a `diff_var` column that tracks the difference between the dispersed variable (see the `var` column) and the reported value. Following the `diff_*` columns in the output of `audit_seq()`, this is the number of dispersion steps, not the actual numeric difference.
-   The same `diff_*` columns are now integer, not double.
-   `function_map()`, `function_map_seq()`, and `function_map_total_n()` have a new `.name_key_result` argument that controls the name of the key result column in the output of the factory-made function. This is `"consistency"` by default, but other names will fit better for other kinds of tests. (The results of these tests must still be logical values.)

## Minor changes

-   In `duplicate_count()`, the `count` column in the output tibble was renamed to `frequency`. This makes for a more streamlined frequency table and removes an ambiguity with `duplicate_count_colpair()`, where the `count` output column means something different.
-   In `seq_disperse()` and `seq_disperse_df()`, the `track_var_change` / `.track_var_change` argument was renamed to `track_diff_var` / `.track_diff_var`. The arguments with the old names are still present for now but will be removed in a future version. Also, the unit of these values is now dispersion steps, for consistency with `grim_map_seq()` etc. as well as `audit_seq()`.
-   `grim_total()`, `grim_ratio()`, and `grim_ratio_upper()` now require `x` to have length 1.
-   The docs now link to functions when opened in RStudio, not just on the website.
-   Accordingly, the output of `write_doc_factory_map_conventions()` now renders links. The function also has a new `scrutiny_prefix` argument for use in another package.
-   The "Infrastructure" article was renamed to "Developer tools"; `vignette("devtools")`.
-   Some dependencies that used to be suggested are now imported.

# scrutiny 0.3.0

## Duplicate analysis overhaul

The `duplicate_*()` functions now present their output better and have overall been streamlined. Read more at `vignette("duplicates")`.

-   A new function, `duplicate_tally()`, marks each observation with its overall frequency. It is similar to `duplicate_detect()` but more informative.

-   In `duplicate_count()`:

    -   All values are now treated like character strings, so all can be checked. The `numeric_only` argument is deprecated and should no longer be used.

    -   The output tibble has two new columns, `locations` and `locations_n`. These hold the names of all input columns in which a value appears and the number of these columns. Details are controlled by the new `locations_type` argument.

    -   New `ignore` argument for specifying one or more values that will not be checked for duplicates.

-   In `duplicate_count_colpair()`:

    -   New `total_x` and `total_y` columns in the output show how many non-missing values were checked for duplicates.

    -   New `ignore` argument as in `duplicate_count()`.

    -   The `na.rm` argument is deprecated. It wasn't very useful because missing values are never checked for duplicates.

-   `duplicate_detect()` is superseded. It is less informative than `duplicate_count()` and, in particular, `duplicate_tally()`. Still, it shares in the overhaul:

    -   As in `duplicate_count()`, all values are now treated like character strings, so all can be checked. The `numeric_only` argument is deprecated and should no longer be used.
    -   The duplicate status of missing values is now shown as `NA`.
    -   New `ignore` argument as in `duplicate_count()`.

## Bugfixes

-   Fixed a numeric precision bug in `round_up_from()` and `round_down_from()` that occurred when rounding numbers greater than circa 2100 with a part to be truncated that was equal to 5 on that decimal level (thanks to \@kaz462, #43). These functions are called within `round_up()` and `round_down()`, and indirectly by all consistency-testing functions.

-   Fixed a bug in `audit_seq()` that displayed one "hit" found by varying a given reported value if there were no such hits. The other columns were not affected.

-   Fixed a bug in `function_map()` that displayed the wrong calling function's name in case of an error.

## Minor improvements

-   Documentation for `grim_map_seq()` and all other functions made by `function_map()`, `function_map_seq()`, or `function_map_total_n()` now displays meaningful defaults. Printing the factory-made functions is more meaningful, as well. Internally, they now work with `rlang::new_function()`, which allows for flexible metaprogramming.

-   The experimental function `audit_list()` is deprecated. Just use `audit()` instead. `audit_seq()` and `audit_total_n()` are now documented separately from `audit()` and `audit_list()`.

-   The lifecycle package is now imported and used in formal deprecations such as that of `sep` in the `restore_*()` functions. The janitor package is no longer suggested.

-   Adjusted to new CRAN requirements for `packageVersion()` usage.

-   Some performance improvements.

# scrutiny 0.2.4

-   New `decimal_places_df()` function that takes a data frame and counts the decimal places in all numeric-like columns.
-   Four new predicate functions centered around `is_map_df()` test whether an object is the output of a scrutiny-style mapper function for consistency tests.
-   Newly exported `is_numeric_like()` function to test whether an object (e.g., a string vector) can be coerced to numeric.
-   New `grim_ratio_upper()` function gives an upper bound for `grim_ratio()`.
-   Changes in `split_by_parens()`:
    -   The function now uses a `cols` argument instead of the dots (`...`). This follows [tidyselect development guidelines](https://tidyselect.r-lib.org/articles/tidyselect.html#selections-as-dots-or-as-named-arguments). The default, `cols = everything()`, is to select all columns that contain the `sep` elements (by default, parentheses). Set the new `check_sep` argument to `FALSE` to select all columns regardless.

    -   All other arguments were renamed: they no longer start on a dot. Furthermore, `.col1` and `.col2` have been renamed to `end1` and `end2`.

    -   A warning is now issued if one or more columns can't be split (or is de-selected from splitting). This occurs if a column doesn't contain the `sep` elements.

    -   Internal changes for compatibility with dplyr 1.1.0.
-   In `restore_zeros_df()` as well, the dots (`...`) were replaced by a `cols` argument, and each other argument no longer has a prefix dot. This follows the changes in `split_by_parens()`, but note the default selection restrictions by the new `check_numeric_like` argument. The optional `check_decimals` argument goes even further.
-   Prevent false-positive warnings when printing ggplot objects (they had occurred since ggplot2 3.4.0).

# scrutiny 0.2.3

Some new features and bugfixes:

-   New `audit()` methods for the output of `audit_seq()` and `audit_total_n()`.

-   New `duplicate_count_colpair()` function that checks each combination of columns in a data frame for duplicates.

-   New `restore_zeros_df()` function to easily restore trailing zeros in all numeric-like columns in a data frame.

-   New `seq_length()` function to extend or shorten linear sequences.

-   Bugfixes in the `is_seq_*()` functions.

-   Argument evaluation is now forced in the function factories: `function_map()`, `function_map_seq()`, and `function_map_total_n()`.

-   Some possible corner case issues in `split_by_parens()` are now prevented.

-   Internal changes for compatibility with purrr 1.0.0 and tidyselect 1.2.0.

# scrutiny 0.2.2

This is a patch for CRAN compliance.

-   The package now requires R version \>= 3.4.0 and rlang version \>= 1.0.2.

-   Subtle changes to `split_by_parens()` that users generally won't notice.

-   Minor shifts in the documentation (e.g., `vignette("consistency-tests")` now has instructions on exporting factory-made functions.).

# scrutiny 0.2.1

This is a patch.

-   It reduces the scope of some examples for CRAN compliance.

-   Minor vignette changes.

# scrutiny 0.2.0

This is a massive release, with many new features and improvements all over scrutiny. Most notably, the package now includes an entirely new system for implementing consistency tests.

-   A new vignette lays out how to implement consistency tests using scrutiny's infrastructure. It describes many of the features mentioned below.
-   GRIMMER support was added, as explained in another new vignette. All GRIM and DEBIT functions mentioned below have GRIMMER analogues. For example, `grimmer_map_seq()` is analogous to `grim_map_seq()`.
-   Because of the new, stricter rules for consistency tests, the output of `grim_map()` no longer includes an `items` column by default. Instead, the numbers of items (1 by default) are factored into the output's `n` column. This focuses the presentation on the essence of GRIM.
-   GRIM and DEBIT functions are now somewhat less likely to flag value sets as inconsistent. That is because measures were taken to reduce spurious, computer-induced differences when comparing floating-point numbers. The same applies to the new GRIMMER functions.
-   `function_map()` enables users to quickly create consistency test functions for data frames much like `grim_map()` or `debit_map()`.
-   `grim_map_seq()` checks if GRIM inconsistencies might be due to small errors, and the true values might be close to the reported ones. It varies the inputs up and down in a specified range, holding the respective other ones constant, and tests all those combinations. For summaries, call `audit_seq()` on the results.
-   `debit_map_seq()` does the same for DEBIT.
-   The above two are powered by `function_map_seq()`, which allows users to easily create functions just like these for any consistency test. All that's needed is a data-frame-level consistency testing function like `grim_map()` or `debit_map()`.
-   `grim_map_total_n()` applies GRIM in cases where no group sizes are reported, only total sample sizes. It systematically matches possible group sizes (around half the total) with reported mean or proportion values, GRIM-tests them, and counts the scenarios in which both matches are consistent. For summaries, call `audit_total_n()` on the results.
-   `debit_map_total_n()` does the same for DEBIT.
-   The above two are powered by `function_map_total_n()`, which allows users to easily create new functions like `grim_map_total_n()` or `debit_map_total_n()`, provided a data-frame-level consistency testing function like `grim_map()` or `debit_map()`.
-   On a lower level still, `disperse_total()` takes a total sample size (comprised of the two unknown group sizes on interest) and calls the appropriate group-level function: `disperse()` for even totals, `disperse2()` for odd ones.
-   `seq_disperse()` and `seq_disperse_df()` extend scrutiny's support for string decimal sequences with trailing zeros. They construct sequences centered around the input; a use case not directly covered by `base::seq()`.
-   Predicate functions around `is_seq_linear()` test whether a vector represents a certain kind of numeric sequence.
-   In `debit_map()`, the `x` column is now to the left of the `sd` column if `show_rec` is `FALSE`, in accordance with the `show_rec = TRUE` default.
-   `debit()` is now vectorized.
-   The functions around `is_subset_of() and is_superset_of()` functions now have stricter variants grouped around `is_proper_subset_of()` and `is_proper_superset_of()`.
-   `split_by_parens()` now accepts any pair of separators passed to `.sep` as a length-2 vector.

# scrutiny 0.1.1

This is a patch, mainly fixing a bug that used to affect the presentation of input data in `grim_map()`'s results. It needs to be emphasized that this bug only affected a convenience feature, namely the presentation of certain input data in the output, not the GRIM test itself.

-   Previously, if `percent` was set to `TRUE`, the `x` values were converted to percentages. Because they need to be presented as strings, percentage conversion involves restoring the correct number of trailing zeros. The bug, then, was that all the `x` values appearing in the output (not in the internal computations!) were restored to the same "length" as the single longest one. This was now remedied, and `x` values are restored to their individually appropriate number of trailing zeros.

-   Another bugfix concerns versioning. Previously, the package had an incorrect version number. It was now corrected.

-   The last change was to remove an outdated and potentially misleading paragraph in the documentation of `reround_to_fraction()`.

# scrutiny 0.1.0

-   This version includes an overhaul of `grim_plot()`:

    -   It extends the function to cover cases of `decimals` values greater than 2, using a gradient instead of a raster.

    -   It enables data-free calls to `grim_plot()` with the new `show_data` argument. Resulting plots only display the background raster. This mirrors Figure 1 in Brown and Heathers' GRIM paper. (Although `grim_plot()` as a whole is modeled after this figure, the default addition of empirical summary data is specific to scrutiny.) Like Brown and Heathers, users may wish to create such raster-only plots in order to demonstrate some principled points. The key parameters `decimals` and `rounding` can be controlled directly to make up for the lack of information from `data`.

    -   The function now checks if all input means or proportions (`x`) have the same number of decimal places. If they don't, it throws an error. This strict criterion can be circumvented by specifying the `decimals` argument. However, since each raster is specific to one number of decimal places (and hence cannot be interpreted regarding `x` values with a different number), the recommended solution is to plot `x` values separately --- once for each number of decimal places.

    -   The `show_full_range` argument was removed because I now think it is superfluous.

    -   Previously, there was some space between the raster and the y-axis. It has now been removed.

    -   Test result data points, shown in blue and/or red by default, are now built on top of the raster, which makes for a more distinct appearance.

-   Two new functions, `reround_to_fraction()`, and `reround_to_fraction_level()`, enable fractional rounding, inspired by `janitor::round_to_fraction()`. For example, they might round `0.4` to `0.5` for fractions of `2`. What tells the new functions apart is that they come with all the flexibility of `reround()`. Furthermore, `reround_to_fraction_level()` is closer to a conventional rounding function than the other two.

-   The new version also fixes a bug in `row_to_colnames()`, rewriting the function's core.

-   Another bug was fixed in `grim()` and `grim_map()`, concerning `show_rec`: For `rounding` strings that lead to four reconstructed numbers per `x` value rather than just two, it used to be the case that only the two values corresponding to the first of the two rounding procedures were displayed in the output tibble. Now, all four are displayed, bearing appropriate names.

-   Another bugfix is for the `threshold` argument in `reround()`, which didn't work properly before. This used to affect higher-level functions such as `grim()`, `grim_map()`, `debit()`, and `debit_map()`, as well. The default for `threshold` is now `5` in all such functions. Note that rounding up and down from 5 has been fully functional independently of it.

-   Also in `reround()`, the `rec` argument has been renamed to `x` in accordance with general naming conventions. The `decimals` argument has been renamed to `digits` in accordance with naming conventions among rounding functions.

-   In `split_by_parens()`, ellipsis support was added to protect the user from silent, unexpected results following named arguments in tidy evaluation. The ellipsis package has been added to the Suggests field in `DESCRIPTION`.

-   In some high-level functions, internal checks now determine if the lengths of multiple arguments that are factored into the same internal function call are mutually congruent. That is, if two such arguments are length \> 1, they need to have the same length (which will throw a warning). Otherwise, there will be an explicit and very specific error message.

-   Finally, some minor refactoring and other small changes that users generally won't notice.

# scrutiny 0.0.1

-   Added vignette about other packages for error detection, called `Related software`.
-   Exported `grim_plot()`.
-   Minor refactoring.

# scrutiny 0.0.0.9000

-   Added a `NEWS.md` file to track changes to the package.
