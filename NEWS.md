# scrutiny 1.0.0

This is scrutiny's first major release. It brings many fixes and new features, but also many breaking changes. If you have used GRIM, GRIMMER, or DEBIT before, you may need to adjust your syntax. You may either read on below or let error messages guide you.

The consistency tests now use exact integer arithmetic, several rounding procedures were corrected, and the mapper functions are now created by `function_map()`. Under the default settings, GRIM's verdicts are unchanged and GRIMMER's differ only in rare edge cases; DEBIT is now exact by default. See "Test results" for details.

## Breaking changes

### Numeric inputs

- `x` and `sd` are now numeric in `grim()`, `grimmer()`, `debit()`, and all their mappers. Strings are no longer accepted. A numeric value does not preserve trailing zeros, so the number of decimal places must be given explicitly: `digits_x` (and `digits_sd` for GRIMMER and DEBIT) are required arguments in every one of these functions. Both accept one value per row in the basic mappers and one value per group in the total-n mappers, as in `grim_map_total_n(df, digits_x = c(2, 1))`.

- The mappers return `digits_x` and `digits_sd` as output columns. `grim_plot()` reads the number of decimal places from these columns instead of inferring it from `x`.

- The `n` column returned by the mappers is now an integer column if all its values are whole numbers within the integer range.

### Test results

- GRIMMER now computes the candidate sums and sums of squares in exact integer arithmetic. Previously, it compared candidates to the rounding bounds in floating point, which could lose a candidate exactly on a bound. Some consistent value sets were therefore reported as inconsistent. For example, `grimmer(x = 0.03, sd = 0.18, n = 200)` was `FALSE`, although a sample of seven 1s and 193 0s has exactly these values. Errors of this kind also affected multi-item scales. Thanks to Ian Hussey (#86) and Aurélien Allard.

- A rounding bound is now treated as exclusive whenever a value exactly on the bound would have been rounded away from the reported number under the given rounding method. This was already the case for `"up"` and `"down"`; it now also applies to `"ceiling"`, `"floor"`, `"trunc"`, `"anti_trunc"`, and the `"*_from"` methods. Under `"even"`, both bounds remain inclusive, because whether a tie occurs under `base::round()` depends on the binary representation of the value.

- `symmetric` now affects the results of `grim()` and `grimmer()`, not just the values shown by `show_rec`. `threshold` no longer affects the results under `"up"`, `"down"`, and `"up_or_down"`, which always round from 5.

- `round_down_from()` now interprets `threshold` in the same way as `round_up_from()`: it rounds down if the part cut off by rounding is at most `threshold` tenths of a rounding step. Previously, it switched direction at `10 - threshold`. As a consequence, the range of `"up_from_or_down_from"` was up to 1.8 steps wide, and GRIM with `threshold = 9` accepted every mean at `n = 57`. Results at the default `threshold = 5` are unchanged.

- `round_anti_trunc()` and `anti_trunc()` no longer change a value that already has the required number of decimal places, so `round_anti_trunc(8.42, digits = 2)` is `8.42`, not `8.43`. This matches Excel's `ROUNDUP()`, Java's `RoundingMode.UP`, and Python's `decimal.ROUND_UP`. A mean reported as `0.00` can therefore be tested under `"anti_trunc"`; the result used to be `NA`.

- DEBIT is exact by default. The mean of `n` binary values is `k / n` for a whole number `k`, but DEBIT treated the mean as a continuous quantity and therefore accepted some SDs that no binary sample of size `n` can have. The new `formula` argument of `debit()` and its mappers selects either `"exact"`, the new default, or `"mean_n"`, the previous behavior. `debit_map()` has two new columns, `sum_lower` and `sum_upper`: the smallest and largest `k` whose mean would be reported as `x`.

- GRIM, GRIMMER, and DEBIT now return `NA` for every case that cannot be tested, under every rounding method. Such cases are: a missing or infinite value, including a missing `digits_*` value; and an `n` or `items` value that is not a positive whole number. For GRIMMER and DEBIT, `n` must be at least 2, since they divide by `n - 1`. Previously, these cases caused an error or returned an arbitrary `TRUE` or `FALSE`. `grim_probability()` returns `NA` for the same cases.

- `grim_probability()`, `grim_total()`, and the `probability` column of `grim_map()` now take the rounding method into account, via new `rounding` and `threshold` arguments. The formula `(10^D - N * L) / 10^D`, with `D` decimal places, sample size `N`, and `L` items, is only correct for rounding methods in which exactly one range of unrounded values, one step wide, leads to each reported value. Under the default `"up_or_down"`, a mean exactly on a tie is accepted in both directions, so the probability is lower: for `n = 40` and two decimal places, it is `0.4`, not `0.6`.

### Function interfaces

- scrutiny uses the base pipe `|>` and no longer exports `%>%`. It requires R >= 4.1.0.

- `grim_map()`, `grimmer_map()`, and `debit_map()` are made by `function_map()`. They now have an explicit argument for every argument of `grim()`, `grimmer()`, and `debit()`; for example, `debit_map()` now has `formula`. The arguments `merge_items`, `extra`, and `testables_only` were removed. Use `dplyr::select()` instead of `extra` and `dplyr::filter(probability > 0)` instead of `testables_only`. `grim_map(percent = TRUE)` no longer prints a message about the conversion.

- `grim()`, `grimmer()`, and `debit()` no longer have `show_rec` or `show_reason`. These arguments were only intended for internal use by the mappers. The three functions are now vectorized only over the tested values (`x`, `sd`, `n`, and the `digits_*` arguments) and `items`; `rounding`, `threshold`, `symmetric`, `percent`, `formula`, `min_val`, `max_val`, and `tolerance` must have length 1. Recycling follows the tidyverse rules: an argument must have length 1 or the common length. `grim(x = c(5.19, 5.18, 5.17), n = c(28, 32), digits_x = 2)` is therefore an error; it used to return three results.

- `grim_map(show_rec = TRUE)` returns the same five columns for every rounding method: `rec_sum`, `sum_lower`, `sum_upper`, `rec_x_upper`, `rec_x_lower`. The `rec_x_*_rounded*` columns are gone. `sum_lower` and `sum_upper` are the smallest and largest whole-number sum totals that would be reported as `x`, and `consistency` is `TRUE` exactly if `sum_lower <= sum_upper`. The displayed values can therefore no longer contradict the result. With `percent = TRUE`, `rec_x_upper` and `rec_x_lower` are percentages, like `x`.

- scrutiny's S3 classes start with `scrutiny_` instead of `scr_`, as in `"scrutiny_grim_map"` (#39).

- The arguments of a mapper call that other functions need later, such as `rounding`, are now recorded in a single list attribute named `"scrutiny"`, not in S3 classes. Classes are now used for method dispatch only. Mapper output no longer has `"scr_rounding_*"` or `"scr_percent_true"` classes, sequence mapper output no longer has the `"scr_map_seq_disp_nonlinear"` class, and the `"scr_seq_df"` and `"scr_seq_test"` classes are gone.

- In `*_map_seq()` output, `case` is now the row number of the value set in the input data. It used to count only the inconsistent rows. If `items` is specified, `n` is now the sample size and `items` a separate column, and `n` is dispersed in steps of 1. `*_map_total_n()` output also returns `n` and `items` separately.

- `reround()` requires `rounding`, `threshold`, and `symmetric` to have length 1, requires `digits` to have length 1 or the length of `x`, and rejects a fractional `digits`. In every function, `rounding` must now be a string and `symmetric` a single `TRUE` or `FALSE`. A numeric or logical `rounding` used to be silently interpreted as a position in the list of rounding methods.

- `decimal_places()`, `decimal_places_scalar()`, `decimal_places_df()`, `restore_zeros()`, and `restore_zeros_df()` match `sep` as a literal string, and the default is `"."` instead of `"\\."`.

- `grim_plot()` and `debit_plot()` return their plot instead of printing it. `grim_plot()` has an aspect ratio of 1; add `ggplot2::theme(aspect.ratio = NULL)` to undo this. `grim_plot()` has a new `split_by_digits` argument that returns a named list with one plot per number of decimal places. `debit_plot()` no longer has the `line_size` argument or the arguments for the outer boxes (`show_outer_boxes` and the `tile_*` arguments). Instead, each rectangle is drawn over a point at the reported mean and SD, so that value sets whose rectangle is too small to see remain visible.

- `duplicate_count_colpair()` requires at least two columns, and its `rate_y` is the proportion of `y` values that occur in `x`, as documented. `audit()` on `duplicate_detect()` output has a row for every column and counts only non-missing values.

## New features

- `is_percentage_of()`, `is_percentage_of_map()`, and `is_percentage_of_map_seq()` check whether a reported percentage matches a reported count out of a reported total. `add_up_to()` and `add_up_to_map()` check whether subgroup sizes sum to a reported total; `add_up_to_map()` selects any number of columns with tidyselect. Both have `audit()` methods.

- `grim_plot_values()` and `grimmer_plot_values()` visualize the test for a single value set. They show every mean (or SD) that could have been reported with the given number of decimal places within a certain range, mark those that a sample of the given size can actually have, and indicate the reported value.

- `grim_plot_rates()` plots each study's rate of GRIM-inconsistent value sets against the rate that random means would show.

- `round_ties_up()`, `round_ties_down()`, `round_ties_away()`, `round_ties_zero()`, and `round_ties_even()`, with matching `rounding` strings `"ties_up"` etc. Each of these names a complete tie-breaking rule: for example, `"ties_away"` is the same as `"up"` with `symmetric = TRUE`. Specifying `symmetric` together with one of these strings is an error. `round_up()`, `round_down()`, and the `"up"` and `"down"` strings continue to work as before.

- `grimmer()` and `grimmer_map()` have new arguments `min_val` and `max_val`: the smallest and largest value that a single response can take, such as the endpoints of a Likert scale. If both are specified, GRIMMER additionally rejects SDs that no sample within these bounds can have, as well as means outside the bounds. `audit()` counts these in a new `fail_scale` column (#60).

- `grim_values()` returns all means that a sample of `n` integers can have and that would be reported as `x`. `grim_closest()` returns the mean that such a sample can have that is closest to `x`, whether or not `x` is GRIM-consistent (#62).

- `grim_binomial()` tests whether the rate of GRIM-consistent value sets is above what random decimal digits would produce, and returns a Clopper-Pearson confidence interval. `grim_binomial_power()` computes the power of this test. Both are experimental.

- `function_map()` gained `.args_by_row`, `.args_defaults`, `.cols_helper`, `.cols_helper_merge`, `.cols_derived`, and a working `.col_names`. With these, `grim_map()`, `grimmer_map()`, and `debit_map()` can be created by `function_map()` without any handwritten code. The new `.reported_variadic` argument is for tests in which the number of key columns is determined by the data rather than by the test (#42). `function_map_seq()` gained `.var_bounds`, which sets separate default limits for each dispersed variable. A mean can therefore be dispersed below zero, while `n` cannot go below 1.

## Bugfixes

### Consistency tests

- `debit()` no longer reports consistent binary data with a mean of exactly `0.50` as inconsistent. It also returns a result for means of exactly 0 and 1, which used to be `NA`.

- The GRIMMER bug that could lead to false positives in test 3 was fixed, and the warning that GRIMMER issued about it was removed (#80). If there are three or more candidate sums of squares under `rounding = "up"` or `"down"`, the SD check and the parity check are now applied to the same candidate. Previously, they could be satisfied by different candidates, so that inconsistent value sets passed (#85).

- `grimmer()` gives the reason `"Negative SD"` for a negative `sd`, and no longer runs indefinitely for a very large `n`. It throws an error if more than a million integer sums are compatible with the reported mean.

- `audit()` no longer counts a case without a test result as inconsistent, and its `mean_grim_prob`, `incons_to_prob`, `mean_x`, and `mean_sd` are now computed over the cases with a test result rather than being `NA` whenever a single case has none.

### Sequence and total-n mappers

- `reverse_map_seq()` and `audit_seq()` now reconstruct the reported values from the `diff_var` column, which records the number of steps between each dispersed value and the reported one. Previously, they inferred the reported value from the position of the gap in the sequence, which failed when `out_min` or `out_max` had cut the sequence short. `audit_seq()` could then test the wrong value and report the opposite result.

- `audit_seq()` re-tests reconstructed data with the same arguments as the original call, including `percent`, `threshold`, `symmetric`, and the scale bounds. It now also works with a `dispersion` of length 1, with output that has no rows, and with cases whose dispersed sequence is empty because of `out_min` or `out_max`.

- In `*_map_seq()` functions, a mean or SD used to be dispersed no lower than one decimal unit above zero; it can now reach zero, and a mean can go below it. With `include_consistent = TRUE`, the functions skip cases that have no test result. If there is nothing to disperse, they return a tibble with no rows but the usual columns.

- `seq_disperse()` now rounds its values to the decimal level of `by`, so that long sequences do not accumulate floating-point error (#83). It applies `out_min` and `out_max` to the whole sequence, including `from` itself, and no longer includes `from` twice when `dispersion` contains 0.

- `*_map_total_n()` functions now use an `items` column, and `reverse_map_total_n()` reconstructs the correct total `n` for any `dispersion`. Functions made by `function_map_total_n()` now work when created in another package (#69).

### Rounding and decimal places

- `decimal_places()` and `decimal_places_scalar()` now handle scientific notation, agree with each other on malformed strings, count numeric values correctly when `sep` is not `"."`, and return `NA` for `NaN`, infinite values, and the string `"NA"`.

- `round_ceiling()`, `round_floor()`, `round_trunc()`, and `round_anti_trunc()` apply the same floating-point tolerance as `round_up()`, so `round_ceiling(0.28, 2)` is `0.28`, not `0.29`. `round_up_from()` and `round_down_from()` now check that `threshold` is valid.

- `unround()` now applies `threshold` consistently, has a `symmetric` argument, reports the correct inclusive and exclusive bounds for `"anti_trunc"` with a negative `x`, checks the lengths of all its arguments against each other, requires `x` to have no more than `digits` decimal places, and accepts only a string or a number as `x`.

- `reround()` now selects the rounding function once per call instead of once per element, which makes it about 200 times faster for long vectors. `threshold = 5` is no longer an error for the `"*_from"` methods. `reround_to_fraction()` applies both procedures of a compound method to every element of `x`, as documented.

- `restore_zeros()` now handles scientific notation, pads with zeros instead of using `sprintf()`, which could introduce floating-point noise at high widths, and returns `NA` rather than the string `"NA"` for missing values. `restore_zeros_df()` and `decimal_places_df()` no longer fail on columns that contain only `NA`, and they now recognize columns whose decimal separator is the one given as `sep_in` or `sep`.

### Other

- `split_by_parens()`, `before_parens()`, and `inside_parens()` now split each string separately. Previously, a string with an unusual number of separators shifted the values of all subsequent rows. Custom separators are matched literally. `split_by_parens()` errors instead of overwriting an existing column, and handles `NA` and 0-row input.

- `duplicate_tally()` no longer counts missing values as matches of every other value, and its running time is now proportional to the number of values rather than to its square. `duplicate_count()` uses a named vector's names as `locations` and drops unused factor levels. `duplicate_count_colpair()` compares values as strings, like the other `duplicate_*()` functions.

- `is_seq_linear()` now returns `FALSE` for sequences such as `c(1, 2, 1)`. `seq_length()` extends a sequence by its own step size and in its own direction. The `is_seq_*()` functions now fill gaps left by `NA` values using the step size implied by the known values. They return `NA` for a vector consisting only of `NA`, whatever its type, and test strings and factors by their numeric values.

- `grim_plot()` plots negative means at the fractional part of their absolute value, plots percentages with the number of decimal places they were tested with, works with `show_raster = FALSE`, and warns about any rows it leaves out instead of suppressing all warnings. `debit_plot()` now draws a separate DEBIT line for each sample size. It used to draw a single line for which the `n` column was recycled.

- Mappers can now be called via `lapply()` or `purrr::map()`, and the name of a key column, such as `x = "mean"`, can be passed from a variable. They evaluate the key arguments instead of inspecting the unevaluated call. `.name_key_result` now works in all three function factories, and `function_map_total_n()` has an `.args_disabled` argument, like the other two. `is_map_df()` and the related predicates now recognize only scrutiny's own classes; they used to accept any class ending in `_map`.

- Errors about an invalid `rounding` or `symmetric` now name the function that the user called, and they are no longer wrapped in `purrr::pmap()`'s "In index" context. Many errors that came from base R or from internal code were replaced by messages that name the argument at fault.

- `grim_map()` is now about 2.5 times as fast, and `grimmer_map()` about 80 times as fast.

## Lifecycle updates

- Removed after their deprecation in scrutiny 0.5.0: the `show_prob` argument of `grim_map()`, `grim_ratio_upper()`, and the 15 functions around `is_subset_of()`.

- Removed after their deprecation in scrutiny 0.3.1: the `track_var_change` argument of `seq_disperse()` and `.track_var_change` of `seq_disperse_df()`. `restore_zeros_df()` and `split_by_parens()` no longer have `...`, which only existed to catch the argument names used before scrutiny 0.2.4.

- The `tolerance` argument of `grim()` and `grim_map()` is deprecated. GRIM compares whole numbers exactly, so a tolerance has no effect. `grimmer()` keeps the argument and uses it.

- `function_map()`'s `.col_control` and `.col_filler` arguments were removed. They never worked.

- The `constant_index` argument of `disperse()`, `disperse2()`, and `disperse_total()` was removed, as was `.constant_index` of `function_map_total_n()` and the resulting `constant_index` of the `*_map_total_n()` functions. The `constant` columns always follow `n_change`; use `dplyr::relocate()` to move them.

- scrutiny requires purrr >= 1.0.0 (#87) and ggplot2 >= 3.4.0. It no longer depends on corrr and suggests ggrepel instead of importing it.

## Documentation

- `vignette("rounding-options")` now states, for each program, the `rounding` string and `symmetric` setting that reproduce its rounding. Two claims were corrected: Stata rounds ties up on the number line, and SPSS Statistics rounds ties away from zero. Excel's `ROUNDUP()` is `"anti_trunc"`, not `"ceiling"`.

- `symmetric` and the floating-point tolerance of the `round_*()` functions are documented in new sections of `?round_up`. `vignette("rounding-in-depth")` was revised, in particular its account of rounding to even.

- `vignette("consistency-tests-in-depth")` was updated (#74), the GRIMMER documentation was improved (#66), and "Related software" has new entries.

- The source code is formatted with [Air](https://posit-dev.github.io/air/).

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
