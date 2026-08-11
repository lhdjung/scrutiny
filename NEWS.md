# scrutiny 1.0.0

## Breaking changes

- scrutiny now uses the base pipe `|>` instead of the magrittr pipe `%>%`, so `%>%` is no longer exported.
- `grim_map()`, `grimmer_map()`, and `debit_map()` are now created by `function_map()` instead of being written by hand. Their output is unchanged, except as noted below, but they gained the remaining arguments of `grim()`, `grimmer()`, and `debit()` -- e.g., `debit_map()` now has a `formula` argument -- and lost these:
  - `grim_map()` and `grimmer_map()` no longer have `merge_items`. Setting it to `FALSE` packed `n` and `items` into a single data-frame column, which every function downstream of the mapper takes to be a numeric vector. The default behavior, multiplying `items` into `n` for the output, is now the only one.
  - `grim_map()` and `debit_map()` no longer have `extra`. It selected which of the other columns of `data` come along, and all three mappers now return all of them. `dplyr::select()` does the same job on the output, with tidyselect, and `grimmer_map()` never had the argument in the first place.
  - `grim_map()` no longer has `testables_only`. Use `dplyr::filter(probability > 0)` on the output instead.
  - `grim_map()` no longer reports "`x` converted from percentage" when `percent` is `TRUE`. The conversion is what the user asked for by setting the argument, and the message fired once per dispersed value inside `grim_map_seq()`.
  - `debit_map()` no longer accepts strings for `x` and `sd`, as `grim_map()` and `grimmer_map()` already didn't. Use numbers and state the decimal places with `digits_x` and `digits_sd`. DEBIT used to count them itself.

- `grim_map(show_rec = TRUE)` now returns the same five columns for every rounding method: `rec_sum`, `sum_lower`, `sum_upper`, `rec_x_upper`, and `rec_x_lower`. The `rec_x_*_rounded*` columns are gone -- four of them for `rounding = "up_or_down"`, `"up_from_or_down_from"`, and `"ceiling_or_floor"`, two for the other methods. They displayed the two reconstructed means re-rounded, which was how GRIM decided consistency before the test moved to exact integer arithmetic in this release. Since the verdict no longer comes from those numbers, they had become a second, parallel reconstruction that could contradict the `consistency` column they were meant to explain: `grim_map(tibble::tibble(x = 2, n = 127), digits_x = 2, rounding = "anti_trunc", show_rec = TRUE)` reported `TRUE` while displaying granules that round to 2.01, and the sum that actually decided the verdict appeared nowhere in the output. The new `sum_lower` and `sum_upper` columns are that deciding range: the least and the greatest whole-number sum total that would have been reported as `x`. A value set is consistent exactly if `sum_lower` is not greater than `sum_upper`, so display and verdict cannot come apart, and the gap between the two says how far off an inconsistent value set is.

- With `percent = TRUE`, the `rec_x_upper` and `rec_x_lower` columns of `grim_map(show_rec = TRUE)` are now percentages, like `x` itself. They used to be the decimal numbers that GRIM converts `x` to internally, so a reported 71 was displayed alongside granules of 0.72 and 0.68 -- values that are documented as the reconstructed `x` values closest to `x`, but that could not be compared to it at a glance. `grim_values()` and `grim_closest()` already returned their values on the scale of `x`. The `rec_sum`, `sum_lower`, and `sum_upper` columns are unchanged: they are sums of the underlying data, which the scale of the reported mean does not change.

- The `n` column returned by `grim_map()`, `grimmer_map()`, and `debit_map()` is now an integer column if all of its values are whole numbers, which sample sizes are. `function_map_seq()` output already followed this convention.

- `grim_map()`, `grimmer_map()`, and `debit_map()` now carry the `digits_x` (and, for GRIMMER and DEBIT, `digits_sd`) they were given forward into a `digits_x` / `digits_sd` output column. The same is true of `*_map_total_n()`.

- `grim_plot()` now reads this column instead of guessing the decimal count from the numeric `x` column via `decimal_places()`, which was unreliable because a numeric value cannot carry trailing zeros: `5.00` reads back as 0 decimal places, not 2.

- `grim_plot()` now has a `split_by_digits` argument (default is `FALSE`) to optionally return a list of plots instead; one plot per distinct number of decimal places.

## Bugfixes

- Fixed an error in the GRIM rounding-boundary logic for `rounding = "up"` and `rounding = "down"`. For `"up"` rounding, the upper boundary is exclusive (a value exactly at the midpoint rounds *away* from the reported mean, not toward it), but the old implementation treated it as inclusive. This could cause `grim()` and `grim_map()` to return `TRUE` for value sets that are actually inconsistent.

- Fixed a bug in GRIMMER that could occur when using the non-default `rounding = "up"` or `"down"` options in cases with three or more candidate sums of squares. In these situations, the checks for the reconstructed SD matching the reported SD and for the sum of squares having the same parity as the sum could be satisfied by different candidate sums instead of the same one, producing false-negative consistency results (i.e., wrongly letting inconsistent value sets pass). Thanks to Ian Hussey (#85).

- GRIM and GRIMMER now derive the set of candidate sums in exact integer arithmetic. Both used to compare quantities against the rounding bounds in floating point, which failed whenever a candidate sat exactly on a bound. In such cases, the two sides of the comparison were different floating-point representations of the same real number. This corrupted verdicts in both directions. `grim()` could therefore pass unattainable means, and `grimmer()` could both pass values with no valid sum of squares and flag mean-SD combinations that could be produced by real data sets. For instance, `grimmer(x = 0.03, sd = 0.18, n = 200)` was reported as inconsistent although the dataset with seven 1s and 193 0s has exactly these values. Thanks to Ian Hussey (#86).

- GRIMMER now derives the bounds of the sum of squares in exact integer arithmetic as well, not just the candidate sums. The previous `round(sum_squares_lower, 12)` correction could not repair floating-point error once the sum of squares exceeded about 1000, because two neighboring doubles are more than 1e-12 apart from there on. A bound that was mathematically an exact integer could then be ceilinged to the next one up, dropping the only viable sum of squares and flagging real data. For instance, `grimmer(x = 7.67, sd = 0, n = 2, items = 3, digits_x = 2, digits_sd = 2)` was reported as inconsistent although two subjects with three items each, all summing to 23, have exactly these values.

- GRIM and GRIMMER now treat a rounding bound as exclusive whenever the rounding method in question would carry a value sitting on it away from the reported number. This was already the case for `rounding = "up"` and `"down"`; it now also holds for `"ceiling"`, `"floor"`, `"ceiling_or_floor"`, `"trunc"`, `"anti_trunc"`, `"up_from"`, `"down_from"`, and `"up_from_or_down_from"`, which used to treat both of their bounds as inclusive and so let some inconsistent value sets pass. `rounding = "even"` keeps both bounds inclusive, since `base::round()` breaks midpoint ties by the parity of the preceding digit, and whether a tie occurs at all depends on the binary representation of the value; being too permissive is the safe direction here.

- `grim()` and `grimmer()` now take `symmetric` into account more comprehensively when deciding consistency. It used to affect only the reconstructed values shown by `grim_map(show_rec = TRUE)`, so that, e.g., `grim(-0.07, n = 40, digits_x = 2, rounding = "up", symmetric = TRUE)` returned `TRUE` even though the only candidate mean, -0.075, rounds to -0.08 under symmetric rounding.

- `threshold` no longer affects `grim()` and `grimmer()` for `rounding = "up_or_down"`, `"up"`, and `"down"`. Those methods round from a fixed 5, as `round_up()` and `round_down()` do, and the documentation already described `threshold` as applying to the `"*_from"` methods only. Passing it alongside `"up"` used to silently move the bounds, producing verdicts that no rounding function in the package would agree with.

- `grimmer()` now accepts `rounding = "ceiling_or_floor"`, `"up_from"`, `"down_from"`, and `"up_from_or_down_from"`. These used to throw an error, because the SD bounds came from `unround()`, which does not know these methods, even though `grim()` and `reround()` both do.

- `round_ceiling()`, `round_floor()`, `round_trunc()`, and `round_anti_trunc()` no longer move a number a whole step because of floating-point error. Shifting a number by `digits` decimal places is inexact -- e.g., `0.28 * 100` is 28.000000000000004 -- so `round_ceiling(0.28, 2)` returned 0.29, and `round_floor(0.29, 2)` returned 0.28. These functions now apply the same tolerance that `round_up_from()` and `round_down_from()` already used.

- `unround()` received many fixes:
  - It now handles `threshold` consistently by no longer contradicting itself by implicitly using a fixed threshold of 5 in, e.g., `round_up()`.
  - It now has a `symmetric` argument, which can influence unrounding behavior.
  - It now treats `rounding = "even"` now treated as inclusive because the bounds cannot be mechanically determined, and widening the range is safer.
  - It now reports the correct boundary inclusion for `rounding = "anti_trunc"` and a negative `x`. The bounds were right, but they carried the inequation signs of the positive case: since anti-truncation rounds away from zero, it is the upper bound that a negative `x` can be reached from, not the lower one.

- `debit()` and `debit_map()` now reconstruct their boundary values the same way as `grim()` and `grimmer()`. DEBIT therefore inherits every fix listed above for `unround()`:
  - It now accepts `rounding = "ceiling_or_floor"`, `"up_from"`, `"down_from"`, and `"up_from_or_down_from"`. These used to throw an error although `debit_map()`'s documentation said that `rounding` is passed on to `debit()`.
  - `threshold` no longer moves the bounds for `rounding = "up_or_down"`, `"up"`, and `"down"`, which round from a fixed 5.
  - `symmetric` now also applies to the reconstruction of the bounds, not just to the re-rounding of the reconstructed SD. DEBIT used to unround asymmetrically and re-round symmetrically within the same call.
  - The reconstructed SD is now compared to the reported SD's range in exact integer arithmetic. DEBIT was the last test to compare bounds in floating point, with a fudge of ±1e-12 in either direction. That fudge also defeated the exclusive bounds of `rounding = "ceiling"`, `"floor"`, `"trunc"`, and the others listed above: a reconstructed SD sitting exactly on such a bound was accepted although the rounding method in question would have carried it away from the reported SD. Some value sets that DEBIT used to pass under these rounding methods are therefore reported as inconsistent now.

- Mapper functions made by `function_map()` no longer return a corrupt tibble when `data` has no rows. The `consistency` column was `NULL`, but the tibble still counted it among its columns.

- A missing value in a key column no longer aborts the tests. `grim()`, `grimmer()`, `debit()`, and their mappers return `NA` for such a case, as they already did for a case whose rounding bounds are undefined. They used to fail from inside an input check with `missing value where TRUE/FALSE needed`, which named neither the column nor the row and read like an internal error rather than like something about the data. Since a mapper tests a whole data frame at once, a single missing value made the other rows untestable too. DEBIT additionally reported a missing value as being outside the range from 0 to 1, because `dplyr::between()` returns `NA` for it. `grimmer_map()` gives such a case the reason `"Missing value"`.

- `debit_map()` now returns `x` and `sd` as numeric columns, not as strings. This matches `grim_map()`.

- Fixed a pre-existing compatibility issue in `debit_plot()` where a theme element was out of date with recent ggplot2 versions.

- `digits_x` and `digits_sd` now accept one value per row, not just a single value for the whole column. `grim_map()`, `grimmer_map()`, and `debit_map()` take a vector of length 1 or `nrow(data)`; the total-n mappers take a length-2 vector stating the decimal places of the two groups, as in `grim_map_total_n(df, digits_x = c(2, 1))`. Data whose values were reported with different numbers of decimal places could not be tested correctly before: any single `digits_x` was wrong for some of the rows. This is what the Bauer and Francis (2021) case study in `vignette("grim")` needs, since it reports means of 4.71 and 5.3. Sequence mappers still require a single value per column, and now say so instead of failing further down.

- `*_map_seq()` functions now take their dispersion step size from `digits_x` / `digits_sd` rather than from the stored value. A mean reported as 5.30 is stored as `5.3`, so it used to be dispersed in steps of 0.1 while a neighboring 4.71 was dispersed in steps of 0.01 -- within the same call, under the same `digits_x`, and with both rows labeled `digits_x = 2` in the output. Among other things, this pushed `debit_map_seq()` values outside the range that DEBIT accepts.

- Fixed the example in the error message for a missing `digits_*` argument. It put the argument inside the data frame -- `grim_map(tibble::tibble(x = 1.4, n = 29, digits_x = 2))` -- so following it reproduced the same error. It now reads `grim_map(tibble::tibble(x = 1.4, n = 29), digits_x = 2)`.

- `seq_disperse()` and `seq_disperse_df()` now keep their sequences on the decimal level given by `by` (or, if `by` is not specified, by `from`). Each value used to be computed in plain floating-point arithmetic, so a long enough `dispersion` vector introduced spurious decimal places: `3.14 - (305 * 0.01)` is `0.0899999999999999`, not `0.09`. This made the sequence mappers fail once `dispersion` reached 305, since the dispersed values then had more decimal places than `digits_x` allowed. The same error could also drop a value that sat exactly on `out_min` or `out_max` (#83).

- `audit_seq()` now works on the output of a `*_map_seq()` function whose underlying mapper has no `digits_*` arguments, such as one built for a test of one's own. `function_map_seq()` added a `digits_*` column for every reported variable except `n`, whether or not the mapper had a matching argument, and `audit_seq()` forwards every such column back to the mapper as an argument. The mapper then rejected its own output. Only variables that the mapper accepts a `digits_*` argument for now get a column, which is unchanged for GRIM, GRIMMER, and DEBIT.

- The errors about missing or misspecified `digits_x` and `digits_sd` arguments now name the function the user actually called, in all of the functions that throw them. The name used to be derived by counting frames up the call stack, which only held for the plain mappers: `Vectorize()` inserts `do.call()` and `mapply()` frames, purrr inserts several of its own, and factory-made functions invoke `fun` as a function object, so that frame carries no name at all. In the worst cases the message was replaced by an internal error -- `debit_map_seq(pigs3)` reported `cannot coerce type 'closure' to vector of type 'character'`, and calling a mapper as `scrutiny::grim_map()` produced `` `what` must be a single string `` -- instead of the guidance the message exists to give. The name is now resolved by finding the outermost consistency test function on the call stack, so sequence and total-n mappers name themselves rather than the basic mapper they call internally.

- `audit_seq()` now finds the mapper that produced its input wherever that mapper is defined. A name that cannot be resolved at all now raises a message saying so.

- Functions made by `function_map_total_n()` now work when they are created outside of scrutiny, e.g., in another package. Their bodies call scrutiny-internal helpers such as `absorb_key_args()`, but the factory used to enclose them in the caller's environment, which has no path to those helpers. They are now enclosed in an environment inheriting from scrutiny's namespace, as those made by `function_map()` and `function_map_seq()` already were (#69).

## New features

- New functions `grim_values()` and `grim_closest()` reconstruct the mean or percentage values that integer data of the reported sample size could actually have produced. `grim_values()` returns every achievable value that would have been reported as `x`, and is empty if `x` is GRIM-inconsistent with `n`; `grim_closest()` returns the single achievable value nearest to `x`, whether or not it is consistent. Like `grim()`, both are vectorized and derive their values in closed form and in exact integer arithmetic. Their goal is similar to `rsprite2::GRIM_test(return_values = TRUE)` (#62).

## Lifycycle updates

- `function_map()` can now do everything that the mappers it creates need, which is why all three of `grim_map()`, `grimmer_map()`, and `debit_map()` are made by it (see above). The factory-made function now has a real argument for every argument of the `*_scalar()` function, with the same default, instead of taking them via the dots. Along with that, the factory gained these arguments:
  - `.args_by_row`, for arguments that may have one value per row of `data`, such as `digits_x`. They become columns of the output.
  - `.args_defaults`, for arguments the mapper should have a different default for than the `*_scalar()` function itself.
  - `.cols_helper` and `.cols_helper_merge`, for arguments that may also be given as columns of `data`, such as `items`.
  - `.col_names`, which replaces the non-functional argument of the same name (see below). It names the columns that the `*_scalar()` function's values unpack into when it is asked to show them, as with `show_rec` or `show_reason`.
  - `.cols_derived`, for columns that the `*_scalar()` function does not return at all but that are computed from the same per-row input, such as `probability` in `grim_map()`, which comes from `grim_probability()`.
  - `.name_class_flags`, for logical arguments that change what the numbers in the output mean and that functions downstream of the mapper therefore need to know about, such as `percent` in `grim_map()`, which `grim_plot()` reads off the `scrutiny_percent_true` class.

- `function_map()`'s experimental `.col_control` and `.col_filler` arguments are gone, and `.col_names` works differently, as described above. The three of them were documented as a way to turn additional values from a `*_scalar()` function into columns, but the code they generated addressed variables that the manufactured function does not have, so any use of them failed. `.col_control` was checked and then never referenced at all.

- scrutiny now requires R >= 4.1.0, as do recent versions of tidyverse packages. This is because the package now uses the base pipe `|>`, but also to avoid any incompatibilities with older versions of R.

- scrutiny now requires purrr >= 1.0.0, which was released in 2022 (#87).

## Documentation

- The `digits_x` and `digits_sd` arguments introduced in 1.0.0 are now documented, and all examples were updated to the numeric `x` and `sd` values that the mappers have taken since then. Many of them still passed strings and omitted the `digits_*` arguments, and so failed to run.

- `grim_plot()`'s examples now pass `digits` explicitly. `grim_plot()` reads the decimal count off the `x` column unless told otherwise, and a numeric column cannot carry trailing zeros -- `pigs1` contains 5.00, which reads back as zero decimal places rather than two.

# scrutiny 0.6.1

## Bugfixes

-   Fixed a floating-point bug in the GRIMMER implementation, thanks to Aurélien Allard. It could treat some consistent value sets as inconsistent because of spurious precision in decimal numbers, at least in some edge cases with multi-item scales. This affected `grimmer()`, `grimmer_map()`, etc.

## Breaking changes

-   In `grim_plot()`, the aspect ratio was set to 1, so the plot is always square now. This is more pleasant to the eye given the background raster. To get the old behavior back, add `ggplot2::theme(aspect.ratio = NULL)` to the plot. You could also choose some ratio other than `NULL`, of course.

## Minor improvements

-   "Related software" (under "Articles" in the header) now has some new entries.

-   The development-focused article "Consistency tests in depth" was updated (#74).

-   GRIMMER documentation was improved (#66).

-   Source code is now formatted using [Air](https://posit-dev.github.io/air/).

-   Internally, the `scr_` prefix used for S3-based operations such as method dispatch was replaced by `scrutiny_` throughout the package for greater clarity and better disambiguation (#39). This change will be invisible to any users who have not previously looked into scrutiny's S3 features.

## Lifecycle updates

### Removed

All of these had been deprecated since scrutiny 0.5.0:

-   The `show_prob` argument of `grim_map()` was removed.

-   `grim_ratio_upper()` was removed.

-   The 15 functions around `is_subset_of()` were removed.

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
