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
